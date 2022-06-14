{
  description = "My NixOS and home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ nixpkgs, home-manager, darwin, ... }:
    let
      username =
        "schneider"; # conveniently, this is my username on all systems.
      # Put things in `specialArgs` that we might need for
      # configuration further down the configuration (for example in
      # hm modules).
      specialArgs = { inherit inputs; };
    in {

      # Configuraiton for MacOS nix-darwin systems.  This helped me
      # write this part of the configuration:
      # https://github.com/shaunsingh/nix-darwin-dotfiles/blob/main/flake.nix
      darwinConfigurations.rombach = let
        system = "x86_64-darwin";
        pkgs = import nixpkgs {
          config.allowUnfree = true; # sorry rms
          inherit system;
        };
      in darwin.lib.darwinSystem {
        inherit pkgs system specialArgs;

        modules = [
          ./hosts/rombach/darwin-configuration.nix
          home-manager.darwinModule
          {
            home-manager = {
              users.${username} = import ./hosts/rombach/home.nix;
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = specialArgs;
            };
          }
        ];
      };

      # Configuration for nixos systems.  Uses the system
      # configuration via the `nixosConfiguration`.  `home-manager` is
      # used as a module (`home-manager.nixosModules.home-manager`).
      nixosConfigurations.anarres = let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          config.allowUnfree = true; # sorry rms
          inherit system;
        };

      in nixpkgs.lib.nixosSystem {
        inherit pkgs system specialArgs;

        modules = [
          ./hosts/anarres/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              users.${username} = import ./hosts/anarres/home.nix;
              useGlobalPkgs = true;
              useUserPackages = false;
              extraSpecialArgs = specialArgs;
            };
          }
        ];
      };

      # This only concerns systems that use home-manager directly (in
      # my case, darwin machines).
      homeConfigurations.${username} = let
        system = "x86_64-darwin"; # Only relevant for darwin.
        pkgs = import nixpkgs {
          config.allowUnfree = true; # Sorry rms
          inherit system;
        };
      in home-manager.lib.homeManagerConfiguration {
        configuration = import ./hosts/rombach/home.nix;

        inherit system username pkgs;
        homeDirectory = "/Users/schneider";
        stateVersion = "22.05";
      };
    };
}
