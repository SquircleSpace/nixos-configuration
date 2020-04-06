{ config, pkgs, ... }:
let
  server = pkgs.minecraft-server.overrideAttrs (oldAttrs: rec {
    version = "1.15.2";
    src = pkgs.fetchurl {
      url = "https://launcher.mojang.com/v1/objects/bb2b6b1aefcd70dfd1892149ac3a215f6c636b07/server.jar";
      sha256 = "12kynrpxgcdg8x12wcvwkxka0fxgm5siqg8qq0nnmv0443f8dkw0";
    };
  });
in
{
  services.minecraft-server = {
    enable = true;
    package = server;
    declarative = true;
    eula = true;
    openFirewall = true;
    serverProperties = {
      broadcast-rcon-to-ops = true;
      view-distance = 20;
      max-build-height = 256;
      allow-nether = true;
      enable-command-block = false;
      gamemode = "survival";
      server-port = 25565;
      enable-rcon = false;
      enable-query = false;
      op-permission-level = 4;
      prevent-proxy-connections = false;
      level-name = "world";
      player-idle-timeout = 0;
      motd = "Minceraft";
      force-gamemode = false;
      hardcore = false;
      white-list = true;
      broadcast-console-to-ops = true;
      pvp = true;
      spawn-npcs = true;
      generate-structures = true;
      spawn-animals = true;
      snooper-enabled = true;
      function-permission-level = 2;
      difficulty = "hard";
      network-compression-threshold = 256;
      level-type = "default";
      max-tick-time = 60000;
      spawn-monsters = true;
      max-players = 20;
      use-native-transport = true;
      enforce-whitelist = false;
      spawn-protection = 16;
      online-mode = true;
      allow-flight = false;
      max-world-size = 29999984;
    };
    whitelist = {
      "Albumpants" = "39ee9808-383a-4d11-a798-71a1adcf6434";
      "drcola2" = "1bc443ab-8d8c-4379-88ae-fa87b5c7561c";
      "daemonabode" = "5358769c-de0e-483e-899f-a30266d52fbf";
      "k00lkev" = "080f5628-af36-441f-89ba-01a31f8bac50";
      "orangesoda31" = "a96011e1-74b2-4e20-9ab5-273c62e10e88";
      "jessicas" = "00b0148a-4574-4190-aaf0-6b596e331e60";
      "CreamyRootBeer" = "596b7705-bd96-4b97-87be-74de85803ea6";
    };
  };
}
