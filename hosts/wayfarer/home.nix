{ inputs, ... }:
{
  imports = [
    ../home.nix
    inputs.mac-app-util.homeManagerModules.default
  ];

}
