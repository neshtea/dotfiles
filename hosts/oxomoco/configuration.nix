{ pkgs, lib, ... }:
{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  imports = [ ./hardware-configuration.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  security.polkit.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    firewall = {
      allowedTCPPorts = [ 17500 ];
      allowedUDPPorts = [ 17500 ];
    };
    hostName = "oxomoco";
    interfaces = {
      enp3s0.useDHCP = true;
      wlp4s0.useDHCP = true;
    };

    networkmanager.enable = true;
    useDHCP = false;
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    openssh.enable = true;
    blueman.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.samsung-unified-linux-driver ];
      allowFrom = [ "all" ];
      browsing = true;
      defaultShared = true;
      openFirewall = true;
    };
    xserver = {
      enable = true;
      desktopManager.gnome.enable = true;
      xkbOptions = "ctrl:swapcaps";
      displayManager.gdm.enable = true;
    };
    tailscale.enable = true;
    plex = {
      enable = true;
      openFirewall = true;
      user = "schneider";
    };
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.schneider = {
    isNormalUser = true;
    home = "/home/schneider";
    shell = "${lib.getExe pkgs.zsh}";
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
      "video"
    ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      vim
      wget
      wireplumber
      gnomeExtensions.pop-shell
    ];
    pathsToLink = [ "/share/zsh" ];
    sessionVariables = rec {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";

      # Not officially in the specification
      XDG_BIN_HOME = "$HOME/.local/bin";
      PATH = [ "${XDG_BIN_HOME}" ];
    };
  };

  virtualisation.docker.enable = true;

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

  };

  powerManagement = {
    enable = true;
  };

  system.stateVersion = "23.05";
}
