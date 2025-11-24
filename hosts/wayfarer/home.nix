{ pkgs, ... }:
{
  imports = [
    ../common.nix
  ];
  home.packages =
    let
      desktopPackages = import ../desktop.nix { inherit pkgs; };
    in
    desktopPackages;
  modules.programs = {
    emacs = {
      enable = true;
      emacsPackage = pkgs.emacs-unstable;
    };
    ghostty.enable = true;
  };
}
