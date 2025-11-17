{ inputs, pkgs, ... }:
{
  imports = [
    ../common.nix
    inputs.mac-app-util.homeManagerModules.default
  ];
  home.packages =
    let
      desktopPackages = import ../desktop.nix { inherit pkgs; };
    in
    desktopPackages;
  modules.programs = {
    emacs = {
      enable = true;
      emacsPackage = pkgs.emacs-git;
    };
    ghostty.enable = true;
  };
}
