{ ... }:
{
  imports = [
    ../common.nix
  ];
  programs.fish.enable = true;
  modules.dev.nix.enable = true;
}
