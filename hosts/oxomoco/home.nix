{ pkgs, ... }:
{
  imports = [ ../home.nix ];

  home.packages = [ pkgs.ghostty ];

  modules.programs.emacs.enable = false;
}
