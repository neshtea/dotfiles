{ config, ... }:

{
  sops.secrets."vaultwarden/admin_token" = { };
  services = {
    vaultwarden = {
      enable = true;
      dbBackend = "sqlite";
      config = {
        DOMAIN = "https://vault.defmarco.com";
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8000;
        SIGNUPS_ALLOWED = true;
        LOG_FILE = "/var/lib/vaultwarden/vaultwarden.log";
      };
      environmentFile = config.sops.secrets."vaultwarden/admin_token".path;
      # environmentFile must contain: ADMIN_TOKEN=...
    };
    caddy.virtualHosts."vault.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}
    '';
  };
}
