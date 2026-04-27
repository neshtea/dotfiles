{ pkgs, inputs, ... }:
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
      pkgs.bitwarden-desktop
    ];

  programs = {
    firefox.enable = true;
    lazygit.enable = true;
  };

  modules = {
    programs = {
      ghostty.enable = true;
      emacs = {
        enable = true;
        emacsPackage = pkgs.emacs-unstable;
      };
    };
    desktop = {
      hyprland.enable = false;
    };
  };

  services = {
    syncthing = {
      enable = true;
    };
  };
}
