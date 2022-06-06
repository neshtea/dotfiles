{ config, pkgs, ... }: rec {
  imports = [ ../home.nix ];

  fonts.fontconfig.enable = pkgs.lib.mkForce true;

  home.packages = with pkgs; [
    # Basic desktop apps
    thunderbird
    linphone # making phone calls
    scrot # Screenshots
    keepassxc
    spotify

    # Chat
    signal-desktop
    element-desktop
    mattermost-desktop
    tdesktop

    # System utilities
    lm_sensors
    pamixer # control pipewire

    ## System gui apps
    # audio controls (meant for pulse, but seems to work
    # with pipewire as well).
    pavucontrol

    # Languages
    racket # includes drracket, raco, etc.

    # desktop utils
    feh

    # Fonts
    iosevka
    fira-code
    font-awesome_5
    jetbrains-mono
    nerdfonts
    noto-fonts
    roboto-mono
    source-code-pro
  ];

  programs = { firefox.enable = true; };

  modules.programs.kitty.enable = true;
  # modules.desktop.herbstluftwm.enable = true;
  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacs28NativeComp;
  };
  modules.desktop.xmonad.enable = true;
  modules.desktop.rofi.enable = true;
  modules.desktop.polybar.enable = true;

  services = {
    dropbox = {
      enable = true;
      path = "${config.home.homeDirectory}/Dropbox";
    };
  };

  xsession = {
    enable = true;
    initExtra = "~/.fehbg";
  };
}
