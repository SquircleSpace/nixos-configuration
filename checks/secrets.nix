{
  "adaTestPassword.age".publicKeys = [
    (builtins.readFile ./test_ssh_host_ed25519_key.pub)
  ];
}
