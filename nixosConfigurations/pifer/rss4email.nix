{ config, lib, ... }:
{
  services.rss4email = {
    enable = true;
    config.feeds = import ./feeds.nix { inherit lib; baseEmail = "rss@squircle.space"; } ;
    config.fromAddress = "rss4email@rss4email.squircle.space";
    config.toEmail = "rss" + "@" + "squircle.spacce";
    config.smtpPasswordFile = "/var/lib/rss4email/smtpPassword";
    config.smtpUsername = "rss4email@rss4email.squircle.space";
    config.smtpServerPort = 587;
    config.smtpServer = "smtp.dreamhost.com";
  };
}
