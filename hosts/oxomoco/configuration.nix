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

    # plasma/kde
    desktopManager.plasma6.enable = true;
    displayManager.sddm.enable = true;
    displayManager.sddm.wayland.enable = true;
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

      # plasma/kde
      kdePackages.kclock
      kdePackages.sddm-kcm
      kdePackages.partitionmanager
      hardinfo2
      wayland-utils
      wl-clipboard
    ];
    plasma6.excludePackages = with pkgs; [
      kdePackages.elisa
      kdePackages.kdepim-runtime
      kdePackages.kmahjongg
      kdePackages.kmines
      kdePackages.konversation
      kdePackages.kpat
      kdePackages.ksudoku
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
