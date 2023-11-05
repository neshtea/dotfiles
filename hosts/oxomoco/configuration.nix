# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "oxomoco"; # Define your hostname.

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    tlp.enable = true;
    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
    };
    xserver = {
      enable = true;
      # libinput = {
      #   enable = true;
      #   mouse.naturalScrolling = true;
      # };
      # layout = "us";
      displayManager = {
        # session = [{
        #   manage = "window";
        #   name = "fake";
        #   start = "";
        # }];
        # defaultSession = "none+fake";
        # autoLogin = {
        #   enable = true;
        #   user = "schneider";
        # };
        gdm = {
          enable = true; 
          wayland = true;
        };
        # lightdm = {
        #   enable = true;
        #   greeters.mini.enable = true;
        #   greeters.mini.user = "schneider";
        # };
        # sessionCommands =
        #   # https://nixos.wiki/wiki/Keyboard_Layout_Customization
        #   let
        #     myCustomLayout = pkgs.writeText "xkb-layout" ''
        #       	    ! Map umlauts to RIGHT ALT + <key>
        #                   keycode 108 = Mode_switch
        #                   keysym e = e E EuroSign
        #                   keysym c = c C cent
        #                   keysym a = a A adiaeresis Adiaeresis
        #                   keysym o = o O odiaeresis Odiaeresis
        #                   keysym u = u U udiaeresis Udiaeresis
        #                   keysym s = s S ssharp
        #           
        #                   ! disable capslock
        #                   ! remove Lock = Caps_Lock
        #       	  '';
        #   in "${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}";
      };
      # xkbOptions = "eurosign:e,caps:ctrl_modifier";
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  # services.printing.drivers = [ pkgs.samsung-unified-linux-driver ];

  # Enable sound.
  sound.enable = true;
  # use pipewire
  # hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.schneider = {
    isNormalUser = true;
    home = "/home/schneider";
    shell = "${lib.getExe pkgs.zsh}";
    extraGroups =
      [ "wheel" "networkmanager" "docker" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      vim
      wget
      # Stuff for wayland/hyprland
      wofi
      waybar
      hyprpaper
      wireplumber
      dunst
      xdg-desktop-portal-wlr
      xdg-desktop-portal-hyprland
      wl-clipboard
      egl-wayland
      qt6.qtwayland
      libsForQt5.qt5.qtwayland
      xwayland
      
      dropbox-cli
    ];
    pathsToLink = [ "/share/zsh" ];
  };

  systemd.user.services.dropbox = {
    description = "Dropbox";
    wantedBy = [ "graphical-session.target" ];
    environment = {
      QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
      QML_IMPORT_PATH = "/run/current/system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
    };
    serviceConfig = {
      ExecStart = "${lib.getBin pkgs.dropbox}/bin/dropbox";
      ExecReload = "${lib.getBin pkgs.coreutils}/bin/kill -HUP $MAINMPID";
      KillMode = "control-group";
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

  virtualisation.docker.enable = true;

  programs = { 
    hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    waybar.enable = true;
  };


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  powerManagement.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment? -- no (:

}

