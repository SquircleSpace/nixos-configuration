let
  keys = import ../publicKeys.nix;
in
{
  "adaHashedPassword.age".publicKeys = [
    keys.systems.plasma
    keys.users.ada.plasma
  ];
}
