{ config, pkgs, ... }: rec {
  imports = [ ../home.nix ];

  fonts.fontconfig.enable = pkgs.lib.mkForce true;

  home.packages = with pkgs; [
    # Basic desktop apps
    firefox
    thunderbird
    keepassxc

    # Chat
    signal-desktop
    element-desktop
    tdesktop

    # System utilities
    lm_sensors
    playerctl # control media applications from the command line.
    rofi-pass
    gcc # sometime you just need a c compiler.

    ## System gui apps
    # audio controls (meant for pulse, but seems to work
    # with pipewire as well).
    pavucontrol

    # desktop utils
    feh

    # Fonts
    iosevka
    fira-code
    font-awesome_5
    jetbrains-mono
  ];

  programs = { firefox.enable = true; };

  modules.programs.kitty.enable = true;
  modules.desktop.herbstluftwm.enable = false;
  modules.programs.neovim.enable = false;
  modules.programs.emacs = {
    enable = false;
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
