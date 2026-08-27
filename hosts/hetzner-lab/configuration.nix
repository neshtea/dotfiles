{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  time.timeZone = "Europe/Vienna";
  i18n.defaultLocale = "en_US.UTF-8";
  networking = {
    hostName = "hetzner-lab";
    useDHCP = false;
    firewall.allowedTCPPorts = [
      22
      80
      443
    ];
  };

  systemd = {
    network = {
      enable = true;
      networks = {
        "10-wan" = {
          matchConfig.Name = "enp1s0";
          networkConfig.DHCP = "ipv4";
          address = [ "2a01:4f8:1c1b:447c::/64" ];
          routes = [ { routeConfig.Gateway = "fe80::1"; } ];
        };
      };
    };
    tmpfiles.rules = [
      "d /var/lib/calibre-web-automated         0750 cwa cwa -"
      "d /var/lib/calibre-web-automated/config  0750 cwa cwa -"
      "d /var/lib/calibre-web-automated/library 0750 cwa cwa -"
      "d /var/lib/calibre-web-automated/ingest  0770 cwa cwa -"
    ];
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
    };
  };

  ############################################
  # fail2ban
  # Note: with SSH already key-only, fail2ban's SSH jail mainly cuts
  # log noise and closed connections rather than stopping a real
  # break-in — but it also covers Caddy and Vaultwarden below, where
  # it's doing real work (both accept password-style logins).
  ############################################
  services.fail2ban = {
    enable = true;
    maxretry = 5;
    bantime = "1h";
    bantime-increment = {
      enable = true; # repeat offenders get progressively longer bans
      maxtime = "168h";
    };
    jails = {
      # Vaultwarden logs failed master-password attempts; this filter
      # matches its "Username or password is incorrect" log line.
      vaultwarden = {
        settings = {
          enabled = true;
          filter = "vaultwarden";
          action = "iptables-allports[name=vaultwarden]";
          logpath = "/var/lib/vaultwarden/vaultwarden.log";
          maxretry = 5;
          bantime = "1h";
        };
      };
    };
  };

  environment.etc."fail2ban/filter.d/vaultwarden.conf".text = ''
    [Definition]
    failregex = ^.*Username or password is incorrect\. Try again\. IP: <HOST>\..*$
    ignoreregex =
  '';

  # Caddy access-log jail: bans IPs hammering any reverse-proxied vhost
  # with 401/403/404s (typical scanner/bruteforce behavior).
  services.caddy.logDir = "/var/log/caddy";
  environment.etc."fail2ban/filter.d/caddy-abuse.conf".text = ''
    [Definition]
    failregex = ^.*"remote_ip":"<HOST>".*"status":(401|403|404).*$
    ignoreregex =
  '';

  # Hardening
  services.journald.storage = "persistent";
  services.journald.extraConfig = "SystemMaxUse=500M";
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = false;
    "net.ipv4.conf.all.accept_redirects" = false;
    "net.ipv4.conf.all.accept_source_route" = false;
    "net.ipv4.tcp_syncookies" = true;
    "net.ipv6.conf.all.accept_redirects" = false;
  };

  # Automatic garbage collection so old generations don't slowly eat
  # disk — doesn't touch running services, purely store cleanup.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  users.mutableUsers = false; # user set is fully declared here, not edited by hand

  users = {
    users =
      let
        sshPublicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDTaoFvrtVyoG1So6OKO0d7SgRzL26xQIpuT0FNfwoT+r2RvF1wlMx7HT6LV0knZKOzIxtTWupHff/YYF/Y73KtGejqRmaSvPI8+/FEcCyveM5ZmgLGuO73sVm8M0ruYuwLMUtm1IjmLnoHOGqZYVT3TcYi8SWRdFaosU9gKR4/oIQ6GONonoQr7TL89vA3aQ+veCfbgEQWc90p1YIuF/I5GsHnqv/rHolGqKNQ3Es9zxNYitxgEPRq6nHeUitzQoK8StzYfhjcSFAUWSBywFmEKH9LjRnmOrMRIjglX/a0+V085NiDuVKfQUKBeSyUQvcq8qT0lTzZDuAvz+icziD51cATYGYWwlpnC+1lKUhzK1IbjJGvangr6gBbh0UOp+lu0snQOe8EYxNLGw5OL9Sxa35724hvs6uYcBNhFRD8WeZgatXwJpBSDsXBOi2CYwBswMNLwUbrasfo7f8lulShHnJV/hvTcXryNiSac7Tt+qGP/La/N53IDnLXe1ewgwxf2vs7IhBVqNOZQCTiolpCZf6+iyz9JYK2kQHVJWMT2bt10PE5RqeW7rQ96Sf36k7ngujsv98R7TVzsTnX2DNyWKHbCy3Ddp7Ksp6xQpWJ3KUoQ1i7BfR6s2gIlxzofwSi3xpP/arUgsZ5Fi9pqOxbO7IwEIwFpT4bw/j8E7nnRQ== marco.schneider@active-group.de";
      in
      {
        root.openssh.authorizedKeys.keys = [ sshPublicKey ];
        marco = {
          isNormalUser = true;
          extraGroups = [ "wheel" ];
          openssh.authorizedKeys.keys = [ sshPublicKey ];
        };
        cwa = {
          isSystemUser = true;
          uid = 3000;
          group = "cwa";
        };
      };
    groups = {
      cwa.gid = 3000;
    };
  };

  security.sudo.wheelNeedsPassword = true;

  programs = {
    git.enable = true;
    vim.enable = true;
  };

  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";

  sops.secrets."navidrome/lastfm_secret" = { };
  sops.secrets."cwa/hardcover_token" = { };
  sops.secrets."vaultwarden/admin_token" = { };
  sops.secrets."smb/credentials" = {
    # rendered to a file on disk so the cifs mount can read it
    path = "/run/secrets/smb-credentials";
  };

  fileSystems."/mnt/music" = {
    device = "//u518967.your-storagebox.de/backup";
    fsType = "cifs";
    options = [
      "credentials=/run/secrets/smb-credentials"
      "x-systemd.automount"
      "x-systemd.mount-timeout=30"
      "_netdev"
      "uid=navidrome"
      "ro"
    ];
  };

  services.syncthing = {
    enable = true;
    user = "marco";
    dataDir = "/home/marco/Sync";
    openDefaultPorts = true;
  };

  services.navidrome = {
    enable = true;
    settings = {
      Address = "127.0.0.1";
      Port = 4533;
      MusicFolder = "/mnt/music";
      ScanSchedule = "1h";
      LogLevel = "info";
      SessionTimeout = "24h";
      BaseUrl = "https://music.defmarco.com";
      LastFM.ApiKey = "42fc64a44dbe3fd134ab3aac391373e2"; # not secret, fine in nix store
      EnableTranscodingConfig = true;
    };
    environmentFile = config.sops.secrets."navidrome/lastfm_secret".path;
    # environmentFile must contain: ND_LASTFM_SECRET=...
  };

  services.vaultwarden = {
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

  virtualisation.podman.enable = true;
  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.calibre-web-automated = {
    image = "ghcr.io/crocodilestick/calibre-web-automated:latest";
    autoStart = true;
    ports = [ "127.0.0.1:8083:8083" ];
    volumes = [
      "/var/lib/calibre-web-automated/config:/config"
      "/var/lib/calibre-web-automated/library:/calibre-library"
      "/var/lib/calibre-web-automated/ingest:/cwa-book-ingest"
    ];
    environment = {
      PUID = "3000";
      PGID = "3000";
      TZ = "Europe/Vienna";
      NETWORK_SHARE_MODE = "false";
      CWA_PORT_OVERRIDE = "8083";
    };
    environmentFiles = [ config.sops.secrets."cwa/hardcover_token".path ];
    # environmentFile must contain: HARDCOVER_TOKEN=...
  };

  services.caddy = {
    enable = true;
    virtualHosts."music.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:4533
    '';
    virtualHosts."vault.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}
    '';
    virtualHosts."calibre.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:8083
    '';
  };

  system.stateVersion = "26.05";
}
