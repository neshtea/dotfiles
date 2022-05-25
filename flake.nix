{
  description = "My nix home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { nixpkgs, home-manager, ... }:
    let username = "schneider";
    in {
      homeConfigurations = {
        x86_64-darwin = let
          system = "x86_64-darwin";
          pkgs = import nixpkgs {
            config.allowUnfree = true;
            inherit system;
          };
        in home-manager.lib.homeManagerConfiguration {
          configuration = import ./x86_64-darwin.nix;

          inherit system username pkgs;
          homeDirectory = "/Users/schneider";
          stateVersion = "22.05";
        };
        x86_64-linux = let
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            config.allowUnfree = true;
            inherit system;
          };
        in home-manager.lib.homeManagerConfiguration {
          configuration = import ./x86_64-linux.nix;

          inherit system username pkgs;
          homeDirectory = "/home/schneider";
          stateVersion = "22.05";
        };
      };
    };
}
