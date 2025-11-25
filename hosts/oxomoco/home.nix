{ pkgs, ... }:
{
  imports = [
    ../common.nix
  ];

  home.packages =
    let
      desktopPackages = import ../desktop.nix { inherit pkgs; };
    in
    desktopPackages
    ++ [
      pkgs.signal-desktop
      pkgs.jetbrains-mono
      pkgs.gcc
      pkgs.mattermost-desktop
      pkgs.thunderbird
    ];

  programs = {
    firefox.enable = true;
    lazygit.enable = true;
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
