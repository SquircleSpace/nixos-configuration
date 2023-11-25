self:
let
  setupModule = { ... }: {
    imports = [
      self.nixosModules.ada
    ];

    age.identityPaths = [
      "/etc/ssh/ssh_host_ed25519_key"
    ];

    # We can't use environment.etc because that runs after both agenix
    # and user/group setup.  This solution is shamelessly stolen from
    # agenix's tests.
    system.activationScripts.agenixInstall.deps = ["installSSHHostKeys"];
    system.activationScripts.installSSHHostKeys.text = ''
      mkdir -p /etc/ssh
      (
        umask u=rw,g=r,o=r
        cp ${./test_ssh_host_ed25519_key.pub} /etc/ssh/ssh_host_ed25519_key.pub
      )
      (
        umask u=rw,g=,o=
        cp ${./test_ssh_host_ed25519_key} /etc/ssh/ssh_host_ed25519_key
      )
    '';
  };
in
{
  name = "ada-account";

  nodes.machineAgePassword = { ... }: {
    imports = [ setupModule ];

    squircle.space.ada = {
      enable = true;
      ageEncryptedHashedPassword = ./adaTestPassword.age;
    };
  };

  nodes.machineFallbackPassword = { ... }: {
    imports = [ setupModule ];

    # Using the default age-encrypted password.  This machine cannot
    # decrypt it since the password hasn't been encrypted with the
    # test machine's key.  As a result, it should use the fallback
    # password.
    squircle.space.ada.enable = true;
  };

  testScript = ''
    start_all()

    machineAgePassword.wait_for_unit("multi-user.target")
    machineAgePassword.succeed("test a$(egrep -o '^ada:[^:]*:' /etc/shadow) = 'aada:$y$j9T$CkMijPV/SVZylz9bYYa9H1$cd1n.uBRwi6cmal4nkcfwqtkrSC56IaU5MWzTYVBSO6:'")

    machineFallbackPassword.wait_for_unit("multi-user.target")
    machineFallbackPassword.succeed("test a$(egrep -o '^ada:[^:]*:' /etc/shadow) = 'aada:$y$j9T$m8cSBKkrKZn/VX52yWu6x0$08knDlnniXOSJmPWLYpEDv4gSHMsPl0kOrFj7vFXs5C:'")
  '';
}
