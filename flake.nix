{
  description = "My NixOS and home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gruvbox-material-kitty = {
      url = "github:rsaihe/gruvbox-material-kitty";
      flake = false;
    };
    kitty-themes = {
      url = "https://github.com/dexpota/kitty-themes";
      flake = false;
    };
    active-kondo = {
      url = "github:active-group/active-kondo";
      flake = false;
    };
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, home-manager, ... }:
    let
      username =
        "schneider"; # conveniently, this is my username on all systems.
      # Put things in `specialArgs` that we might need for
      # configuration further down the configuration (for example in
      # hm modules).
      specialArgs = { inherit inputs; };
    in {
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
        system = "aarch64-darwin"; # Only relevant for darwin.
        # overlays = [ inputs.neovim-nightly-overlay.overlay ];
        overlays = [ ];
        pkgs = import nixpkgs {
          config.allowUnfree = true; # Sorry rms
          inherit overlays system;
        };
      in home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = specialArgs;
        modules = [
          ./hosts/wayfarer/home.nix
          {
            home = {
              inherit username;
              homeDirectory = "/Users/schneider";
              stateVersion = "22.05";
            };
          }
        ];
      };
    };
}
