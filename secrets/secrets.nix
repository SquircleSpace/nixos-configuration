let
  keys = import ../lib/publicKeys.nix;
in
{
  "adaHashedPassword.age".publicKeys = [
    keys.systems.pifer
    keys.systems.plasma
    keys.users.ada.plasma
  ];
}
