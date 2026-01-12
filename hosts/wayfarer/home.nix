{ pkgs, inputs, ... }:
{
  imports = [
    ../common.nix
    inputs.nix-starter-kit.homeModules.khard
    inputs.nix-starter-kit.homeModules.timetracking
  ];
  home.packages =
    let
      desktopPackages = import ../desktop.nix { inherit pkgs; };
    in
    desktopPackages
    ++ [
      pkgs.lazygit
    ];
  active-group = {
    timetracking = {
      enable = true;
      timetracking-token = "/Users/schneider/.timetracking/timetracking.key";
      arbeitszeiten-token = "/Users/schneider/.timetracking/arbeitszeiten.key";
      abrechenbare-zeiten-token = "/Users/schneider/.timetracking/abrechenbare-zeiten.key";
    };
    khard = {
      enable = true;
      storagePath = "/Users/schneider/repo/active-group/addresses/vcf";
    };
  };
  modules.programs = {
    emacs = {
      enable = true;
      emacsPackage = pkgs.emacs-unstable;
    };
    ghostty.enable = true;
  };
}
