rec {
  users.ada.jobe = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDnFNhCt7SNYQrkpQV4dfFoIk3ec76P7xBeReLj8XKlK";
  users.ada.libbie = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBl+eRelWQ6BNroWtx+Tdl0fuEcy19ZHIEn1XQLhdTT1";
  users.ada.plasma = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKXryyeR1i9zTWAY+IMu1yblLFvSewm4p/Y3WkWdZS+z";
  users.ada.phone = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOVLYUsgtyGnUn3vCChhRJEyWpo3CFmLdYyR/3dIZLn8KsVmtbYP8ZPMi95pzPUshnxGxxV1N6MBL8bewM2BeCM=";
  users.ada.photosync = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL0ZZVdNVBVhNKQoibirCEzdyseqStY4qhnwdZ5X13XZ";

  systems.plasma = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF8o4UTI6QggS1xTlLJI/uoy/fFvLf2kQ7rgwMTGxnX4";
  systems.pifer = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZoNhpEnymFLYZqDyEWg/34QPiSTuY0BG7QuB2HCfMV";

  trustedClients = with users.ada; [
    jobe
    libbie
    plasma
  ];
}
