{ pkgs, ... }:
{
  imports = [ ../home.nix ];

  home.packages = [
    pkgs.firefox
    pkgs.ghostty
    pkgs.jetbrains-mono
    pkgs.gcc
  ];

  modules.programs.emacs.enable = false;
  modules.desktop = {
    hyprland.enable = true;
    rofi.enable = true;
  };
  dconf.enable = true;
}
