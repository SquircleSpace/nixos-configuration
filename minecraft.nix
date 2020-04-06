{ pkgs }:
pkgs.minecraft.overrideAttrs (oldAttrs: rec {
  version = "2.1.11314";
  src = pkgs.fetchurl {
    url = "https://launcher.mojang.com/download/linux/x86_64/minecraft-launcher_${version}.tar.gz";
    sha256 = "1wd3zh91zamlpgnqlk7sq3xja2g5qz34amy4v8yhdxkhj79plwhg";
  };
})
