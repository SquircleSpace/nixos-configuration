# This test ensures that user password options have the priority order
# I expect in nixosModules/ada.nix.
let
  baseAdaConfig = {
    isNormalUser = true;
    home = "/home/ada";
  };

  passwordConfigs = pkgs: {
    password = "a";
    hashedPassword = "$y$j9T$CkMijPV/SVZylz9bYYa9H1$cd1n.uBRwi6cmal4nkcfwqtkrSC56IaU5MWzTYVBSO6"; # b
    passwordFile = "${pkgs.writeText "password.txt" "$y$j9T$eutg53BKZ.3RAJFI1DaBz.$Bq1UtynN/W3olWfipTn2GCrKfD5cPtP7tw1LuLS81UD"}"; # c
  };
  passwords = {
    password = "a";
    hashedPassword = "b";
    passwordFile = "c";
  };

  adaConfigWithPasswordFields = pkgs: keys: baseAdaConfig // builtins.foldl' (l: r: l // r) {} (builtins.map (k: {"${k}" = (passwordConfigs pkgs)."${k}";}) keys);
  configWithPasswordFields = pkgs: keys: {
    users.users.ada = adaConfigWithPasswordFields pkgs keys;
  };

  scriptForMachineAndKeys = machineName: key: ''
    ${machineName}.wait_for_unit("multi-user.target")
    ${machineName}.send_chars("ada\n")
    ${machineName}.sleep(1)
    ${machineName}.send_chars("${passwords."${key}"}\n")
    ${machineName}.sleep(1)
    ${machineName}.send_chars("touch imin\n");
    ${machineName}.sleep(1)
    ${machineName}.succeed("test -e /home/ada/imin")
  '';
in
{
  name = "password-priority-order";

  nodes.machine1 = {config, pkgs, ...}: configWithPasswordFields pkgs ["password" "hashedPassword"];
  nodes.machine2 = {config, pkgs, ...}: configWithPasswordFields pkgs ["password" "passwordFile"];
  nodes.machine3 = {config, pkgs, ...}: configWithPasswordFields pkgs ["hashedPassword" "passwordFile"];
  nodes.machine4 = {config, pkgs, ...}: configWithPasswordFields pkgs ["password" "hashedPassword" "passwordFile"];

  testScript = builtins.concatStringsSep "\n" [
    (scriptForMachineAndKeys "machine1" "password")
    (scriptForMachineAndKeys "machine2" "passwordFile")
    (scriptForMachineAndKeys "machine3" "passwordFile")
    (scriptForMachineAndKeys "machine4" "passwordFile")
  ];
}
