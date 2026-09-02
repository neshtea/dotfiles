{ config, pkgs, ... }:
{
  services.cockpit = {
    enable = true;
    port = 9090;
    plugins = [
      pkgs.cockpit-files
      pkgs.cockpit-podman
      pkgs.cockpit-machines
    ];
    allowed-origins = [ "https://cockpit.defmarco.com" ];
    settings = {
      WebService = {
        AllowUnencrypted = true;
        ProtocolHeader = "X-Forwarded-Proto";
      };
    };
  };
  services.caddy = {
    enable = true;
    virtualHosts."cockpit.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:${toString config.services.cockpit.port}
    '';
  };
}
