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
    let
      system = "x86_64-darwin";
      username = "schneider";
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit system;
      };
    in {
      homeConfigurations.${username} =
        home-manager.lib.homeManagerConfiguration {
          configuration = import ./home.nix;

          inherit system username pkgs;
          homeDirectory = "/Users/${username}";
          stateVersion = "22.05";
        };
    };
}
