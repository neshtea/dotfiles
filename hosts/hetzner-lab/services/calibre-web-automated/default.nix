{
  config,
  smbDevice,
  smbCredentialsFile,
  ...
}:
{
  sops.secrets."cwa/hardcover_token" = { };
  systemd = {
    tmpfiles.rules = [
      "d /var/lib/calibre-web-automated/config  0750 cwa cwa -"
      "d /var/lib/calibre-web-automated/ingest  0770 cwa cwa -"
    ];
    services.podman-calibre-web-automated.unitConfig.RequiresMountsFor = "/mnt/books";
  };
  fileSystems =
    let
      credentials = "credentials=${smbCredentialsFile}";
    in
    {
      "/mnt/books" = {
        device = "${smbDevice}/Books";
        fsType = "cifs";
        options = [
          credentials
          "x-systemd.mount-timeout=30"
          "_netdev"
          "nofail"
          "uid=cwa"
          "gid=cwa"
          "file_mode=0660"
          "dir_mode=0770"
          "nobrl"
        ];
      };
    };
  virtualisation = {
    podman.enable = true;
    oci-containers = {
      backend = "podman";
      containers.calibre-web-automated = {
        image = "ghcr.io/crocodilestick/calibre-web-automated:latest";
        autoStart = true;
        ports = [ "127.0.0.1:8083:8083" ];
        volumes = [
          "/var/lib/calibre-web-automated/config:/config"
          "/mnt/books:/calibre-library"
          "/var/lib/calibre-web-automated/ingest:/cwa-book-ingest"
        ];
        environment = {
          PUID = "3000";
          PGID = "3000";
          TZ = "Europe/Vienna";
          NETWORK_SHARE_MODE = "true";
          CWA_PORT_OVERRIDE = "8083";
        };
        environmentFiles = [ config.sops.secrets."cwa/hardcover_token".path ];
        # environmentFile must contain: HARDCOVER_TOKEN=...
      };
    };
  };
  services.caddy = {
    enable = true;
    virtualHosts."calibre.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:8083
    '';
  };
}
