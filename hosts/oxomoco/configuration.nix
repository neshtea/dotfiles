{
  inputs,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ./hardware-configuration.nix ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true; # noop
    efi.canTouchEfiVariables = true;
  };
  security.polkit.enable = true;
  security.rtkit.enable = true;
  security.sudo = {
    wheelNeedsPassword = true;
    execWheelOnly = true;
  };
  zramSwap.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Vienna";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    firewall = {
      # Syncthing ports.
      allowedTCPPorts = [ 22000 ];
      allowedUDPPorts = [ 21027 ];
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
    # blueman.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.samsung-unified-linux-driver ];
      allowFrom = [ "all" ];
      browsing = true;
      defaultShared = true;
      openFirewall = true;
    };
    tailscale.enable = true;

    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;

    udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.schneider = {
    isNormalUser = true;
    home = "/home/schneider";
    shell = "${lib.getExe pkgs.fish}";
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
      gnome.adwaita-icon-theme
      gnomeExtensions.appindicator
    ];
    # Optional, hint electron apps to use wayland:
    sessionVariables.NIXOS_OZONE_WL = "1";
  };

  virtualisation = {
    docker.enable = true;
    podman.enable = true;
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    hyprland = {
      enable = false;
      package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      portalPackage =
        inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
    };

    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };

  powerManagement = {
    enable = true;
  };

  system.stateVersion = "23.05";
}
