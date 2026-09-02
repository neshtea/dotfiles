{
  ...
}:

let
  smbDevice = "//u518967.your-storagebox.de/backup";
  smbCredentialsFile = "/run/secrets/smb-credentials";
in
{
  imports = [
    ./hardening.nix
    ./hardware-configuration.nix
    ./services/calibre-web-automated
    ./services/cockpit
    ./services/navidrome
    ./services/vaultwarden
  ];

  _module.args = { inherit smbDevice smbCredentialsFile; };

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
          routes = [ { Gateway = "fe80::1"; } ];
        };
      };
    };
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
    };
  };

  # Automatic garbage collection so old generations don't slowly eat
  # disk — doesn't touch running services, purely store cleanup.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  users.mutableUsers = true;

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

  sops = {
    defaultSopsFile = ./secrets.yaml;
    age.keyFile = "/var/lib/sops-nix/key.txt";
    secrets."smb/credentials" = {
      # rendered to a file on disk so the cifs mount can read it
      path = smbCredentialsFile;
    };
  };

  services.syncthing = {
    enable = true;
    user = "marco";
    dataDir = "/home/marco/Sync";
    openDefaultPorts = true;
  };

  services.caddy.enable = true;

  system.stateVersion = "26.05";
}
