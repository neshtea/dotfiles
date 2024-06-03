{ pkgs, ... }: {
  imports = [ ../home.nix ];

  fonts.fontconfig.enable = pkgs.lib.mkForce true;

  home.packages = with pkgs; [
    # Basic desktop apps
    brave
    keepassxc
    dolphin

    # Chat
    signal-desktop
    element-desktop
    tdesktop

    # Streaming
    obs-studio
    obs-studio-plugins.wlrobs

    # Music
    plexamp

    # System utilities
    lm_sensors
    playerctl # control media applications from the command line.
    gcc # sometime you just need a c compiler.

    ## System gui apps
    # audio controls (meant for pulse, but seems to work
    # with pipewire as well).
    pavucontrol
    networkmanagerapplet

    # Fonts
    iosevka
    fira-code
    font-awesome_5
    jetbrains-mono
  ];

  modules.programs.kitty.enable = false;
  modules.programs.wezterm.enable = true;

  # Desktop stuff
  modules.desktop.hyprland.enable = false;
  modules.desktop.waybar.enable = false;
  modules.desktop.wofi.enable = false;

  modules.desktop.herbstluftwm.enable = false;
  modules.programs.neovim.enable = true;
  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacs-git;
  };
  modules.desktop.xmonad.enable = false;
  modules.desktop.rofi.enable = false;
  modules.desktop.polybar.enable = false;

}
