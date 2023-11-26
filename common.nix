{ config, pkgs, lib, ... }:
{
  imports = [
    ./flake-autoupdate.nix
    ./backup.nix
  ];
  config = {
    # Manage that network
    networking.networkmanager.enable = lib.mkDefault true;

    # Firejail the things
    programs.firejail.enable = lib.mkDefault true;

    # Keep tmp temporary!
    boot.tmp.useTmpfs = lib.mkDefault true;
    boot.tmp.cleanOnBoot = lib.mkDefault true;

    programs.gnupg.agent.enable = lib.mkDefault true;
    programs.gnupg.agent.enableSSHSupport = lib.mkDefault true;

    # No sudo!
    security.sudo.enable = lib.mkDefault false;
    security.doas.enable = lib.mkDefault true;
    security.doas.extraRules = [
      {
        groups = [ "wheel" ];
        persist = true;
        keepEnv = true;
      }
    ];

    # Nix flakes requires git to be in the PATH.  Its silly.
    environment.systemPackages = [ pkgs.gitFull ];

    # Enable flakes!
    nix = {
      package = lib.mkDefault pkgs.nixUnstable;
      extraOptions = ''
      experimental-features = nix-command flakes
    '';
    };

    # Trust myself
    security.pki.certificates = [
      ''
        -----BEGIN CERTIFICATE-----
        MIIDPDCCAiSgAwIBAgIUSp6sao5P28Pmvr5oHL9Ot8yT6R4wDQYJKoZIhvcNAQEL
        BQAwETEPMA0GA1UEAwwGQWRhIENBMB4XDTIyMDkyMjA5MDkzN1oXDTMyMDkxOTA5
        MDkzN1owETEPMA0GA1UEAwwGQWRhIENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8A
        MIIBCgKCAQEAtMEwUhBoXciObFCEyypO/M8XEb5YOPuvCy5rCt8BToVc7NyE9swt
        zfTQ6vFdMzDc0ZVc9sQbg37R+w+5a/50PCw5IYHcUFxYQHfBZ33PMUHdx7Rn0P0K
        QnIhTkOH3cwoAsw+iOdLV69OTMhbL9uqMh2p9X2JWraxLPGLlIATvMNaA9iFXiFg
        3Klv+pgoNNym9C1RWkdfy1crFKttqLu+LfLEgXlXPnWUh0brf58V1pfQUKnqVP+h
        JpRoUVOjb69jQ+PBicFnI/W2sNvk9s+Gmo24KhZ+CjVBrJ98Fez1X20e+udo/DR2
        rhoX0hQolcGbGzq2nFF2gOZlEFcHx0n+gQIDAQABo4GLMIGIMB0GA1UdDgQWBBRR
        TdlNMk5KpN8LPEwqkQE9jJYOPzBMBgNVHSMERTBDgBRRTdlNMk5KpN8LPEwqkQE9
        jJYOP6EVpBMwETEPMA0GA1UEAwwGQWRhIENBghRKnqxqjk/bw+a+vmgcv063zJPp
        HjAMBgNVHRMEBTADAQH/MAsGA1UdDwQEAwIBBjANBgkqhkiG9w0BAQsFAAOCAQEA
        lDXIMwa860Vi1I4DTkUKe1YMLhx5qlP4McLeq+BlklZlBa15awtYvPbFOgGyUfO/
        m58kNHkWF7Cia8BDTZCMmv02D0KINQHE492auVBSOS/JhbNlZmYmYeY8S3a3zRV4
        VqiEP0b3blZk7EXF0lgE37hGun4K19HTkRKQQMMyImCQUn4d+4dCyF2hGGvX/0df
        UYqLMAg7dR9VVbMKV8FhCWP8vyHtirUSVTMVoB46chKbN0Q1x86YXxidQgT//3gP
        0jiIbGMF6yP3IGBUMZpxzbcWGNsglAaSGbjKMM5f7tbSU0z1AVzSNSdpfDI6YQF2
        H4CJJAA4xmUFREVQQMBCZA==
        -----END CERTIFICATE-----
      ''
    ];
  };
}
