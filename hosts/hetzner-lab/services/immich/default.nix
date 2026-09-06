{ config, smbDevice, smbCredentialsFile, ... }:
{
  fileSystems."/mnt/photos" = {
    device = "${smbDevice}/Photos";
    fsType = "cifs";
    options = [
      "credentials=${smbCredentialsFile}"
      "x-systemd.mount-timeout=30"
      "_netdev"
      "nofail"
      "uid=immich"
      "gid=immich"
      "file_mode=0660"
      "dir_mode=0770"
      "nobrl"
    ];
  };
  systemd.services.immich-server.unitConfig.RequiresMountsFor = "/mnt/photos";
  services = {
    immich = {
      enable = true;
      host = "127.0.0.1";
      mediaLocation = "/mnt/photos";
      openFirewall = true;
    };
    caddy.virtualHosts."photos.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:${toString config.services.immich.port}
    '';
  };
}
