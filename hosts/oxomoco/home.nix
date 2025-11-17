{ pkgs, ... }:
{
  imports = [ ../home.nix ];

  home.packages = [
    pkgs.firefox
    pkgs.ghostty
    pkgs.jetbrains-mono
    pkgs.gcc
    pkgs.mattermost-desktop
    pkgs.thunderbird
  ];

  modules.programs.emacs.enable = false;
  modules.desktop = {
    hyprland.enable = true;
  };
  dconf.enable = true;
}
