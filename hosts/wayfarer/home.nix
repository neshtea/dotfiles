{ inputs, pkgs, ... }:
{
  imports = [
    ../home.nix
    inputs.mac-app-util.homeManagerModules.default
  ];
  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacs-git;
  };
}
