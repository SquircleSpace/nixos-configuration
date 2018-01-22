{ config, pkgs, ... }:
let
  linuxPackages = pkgs.linuxPackages_4_14;
  applespiPackage = import ./applespi.nix { linuxPackages = linuxPackages; inherit pkgs; };
  desiredLinuxPackages = linuxPackages // { applespi = applespiPackage; };
in
{
  boot.kernelPackages = desiredLinuxPackages;
  boot.extraModulePackages = [ applespiPackage ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "beep";

  time.timeZone = "America/Los_Angeles";

  environment.systemPackages = with pkgs; [
    applespiPackage
  ];

  programs.bash.enableCompletion = true;

  services.xserver.layout = "us";

  services.xserver.libinput.enable = true;

  services.xserver.displayManager.sddm.autoLogin = {
    enable = true;
    user = "brad";
  };

  system.stateVersion = "17.09";

  boot.initrd.availableKernelModules = [ "ahci" "ohci_pci" "ehci_pci" "pata_atiixp" "xhci_pci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "spi_pxa2xx_platform" "spi_pxa2xx_pci" "intel_lpss_pci" "applespi" ];
  boot.kernelModules = [ "kvm-amd" ];
}
