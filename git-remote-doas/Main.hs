import GHC.Environment
import Data.Map as Map
import Data.Set as Set
import Data.List
import Data.Maybe
import System.IO
import System.Exit
import System.Environment
import System.Posix.User
import System.Posix.Process
import System.Directory
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class ()

data Args = Args
  { argsAlias :: String
  , argsURL :: String
  , argsRaw :: [String]
  }

parseArgs :: [String] -> Either String Args
parseArgs raw@(first:second:[]) = return $ Args first second raw
parseArgs _ = Left "Invalid arguments"

type CommandResult = ReaderT Args (ExceptT String IO) ()
type Command = String -> CommandResult
type CommandTable = Map.Map String Command

capabilities :: [String]
capabilities =
  [ "connect"
  ]

capabilitiesCommand :: Command
capabilitiesCommand _ = do
  liftIO $ putStrLn $ concat $ intersperse "\n" capabilities
  liftIO $ putStrLn ""
  liftIO $ hFlush stdout
  return ()

withoutPrefix :: Eq a => [a] -> [a] -> Maybe [a]
withoutPrefix full [] = Just full
withoutPrefix [] (_:_) = Nothing
withoutPrefix (a:fullRest) (b:prefixRest)
  | a == b = withoutPrefix fullRest prefixRest
  | otherwise = Nothing

allowedServices :: Set.Set String
allowedServices = Set.fromList
  [ "git-upload-pack"
  , "git-receive-pack"
  ]

connectCommand :: Command
connectCommand service = do
  unless (Set.member service allowedServices) $ do
    throwError $ "Invalid service: " ++ service
  mangledService <- fromMaybe (throwError $ "Invalid service: " ++ service)
                    $ fmap return
                    $ withoutPrefix service "git-"
  env <- Map.fromList <$> liftIO getEnvironment
  args <- ask
  let (_, path) = parseURL (argsURL args)
  let newEnv = Map.delete "GIT_DIR" env
  liftIO $ putStrLn ""
  liftIO $ hFlush stdout
  pid <- liftIO $ forkProcess $ do
    setCurrentDirectory path
    liftIO $ executeFile "git" True [mangledService, path] $ Just $ Map.toList newEnv
  _ <- liftIO $ getProcessStatus True False pid
  throwError ""

commandTable :: CommandTable
commandTable = Map.fromList
  [ ("capabilities", capabilitiesCommand)
  , ("connect", connectCommand)
  ]

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn _ [] = ([], [])
splitOn sigil (c:rest)
  | c == sigil = ([], rest)
  | otherwise = (c:front, back)
  where (front, back) = splitOn sigil rest

splitCommand :: String -> (String, String)
splitCommand = splitOn ' '

defaultCommand :: String -> Command
defaultCommand command _ = throwError $ "Unrecognized command: " ++ command

lookupCommand :: String -> Command
lookupCommand name = fromMaybe (defaultCommand name) $ Map.lookup name commandTable

handleLine :: String -> CommandResult
handleLine line = let
  (commandName, args) = splitCommand line
  command = lookupCommand commandName
  in command args

handleCommandLeft :: String -> IO (ExitCode)
handleCommandLeft str = do
  let isBlank = str == ""
  unless isBlank $ hPutStrLn stderr str
  return $ if isBlank then ExitSuccess else ExitFailure 1

mainLoop_ :: Args -> IO (ExitCode)
mainLoop_ args = do
  line <- getLine
  let commandResult = handleLine line
  result <- runExceptT $ runReaderT commandResult args
  either handleCommandLeft (\_ -> mainLoop args) result

mainLoop :: Args -> IO (ExitCode)
mainLoop args = do
  eof <- isEOF
  if eof then return ExitSuccess else mainLoop_ args

parseURL :: String -> (String, String)
parseURL = splitOn '@'

isCorrectUser :: Args -> String -> Bool
isCorrectUser args username = desiredUser args == username

desiredUser :: Args -> String
desiredUser args = result
  where (result, _) = parseURL $ argsURL args

main :: IO ()
main = do
  rawArgs <- getArgs
  let giveUp = (\err -> hPutStrLn stderr err >> exitWith (ExitFailure 2))
  args <- either giveUp return $ parseArgs rawArgs
  username <- getEffectiveUserName
  unless (isCorrectUser args username) $ do
    allArgs <- getFullArgs
    executeFile "doas" True (["-u", desiredUser args] ++ allArgs) Nothing
  code <- mainLoop args
  exitWith code
