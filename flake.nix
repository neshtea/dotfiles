{
  description = "My nix home-manager configuration as a flake.";

  inputs = {
    home-manager.url = "github:nix-community/home-manager";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { home-manager, ... }:
    let
      system = "x86_64-darwin";
      username = "schneider";
    in {
      homeConfigurations.${username} =
        home-manager.lib.homeManagerConfiguration {
          configuration = import ./home.nix;

          inherit system username;
          homeDirectory = "/Users/${username}";
          stateVersion = "22.05";
        };
    };
}
