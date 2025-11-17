{ pkgs, ... }:
{
  imports = [ ../home.nix ];

  home.packages = [
    pkgs.firefox
    pkgs.jetbrains-mono
    pkgs.gcc
    pkgs.mattermost-desktop
    pkgs.thunderbird
  ];

  programs = {
    foot = {
      enable = true;
      settings = {
        main = {
          term = "xterm-256color";
          font = "JetBrains Mono:size=11";
          shell = "fish";
        };
      };
    };
  };

  modules.programs.emacs.enable = false;
  modules.desktop = {
    hyprland.enable = true;
  };
  dconf.enable = true;
}
