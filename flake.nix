{
  description = "My NixOS and home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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
      nixosConfigurations.oxomoco = let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          config.allowUnfree = true; # sorry rms
          inherit system;

          overlays = [ inputs.emacs-overlay.overlays.default ];
        };

      in nixpkgs.lib.nixosSystem {
        inherit pkgs system specialArgs;

        modules = [
          ./hosts/oxomoco/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              users.${username} = import ./hosts/oxomoco/home.nix;
              useGlobalPkgs = true;
              useUserPackages = false;
              extraSpecialArgs = specialArgs;
            };
          }
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490
        ];
      };

      # This only concerns systems that use home-manager directly (in
      # my case, darwin machines).
      homeConfigurations.${username} = let
        system = "aarch64-darwin"; # Only relevant for darwin.
        # overlays = [ inputs.neovim-nightly-overlay.overlay ];
        overlays = [
          inputs.emacs-overlay.overlays.default
          inputs.neovim-nightly-overlay.overlay
        ];
        pkgs = import nixpkgs {
          config.allowUnfree = true; # Sorry rms
          inherit overlays system;
          # Use this flake.nix's nixpkgs for stuff like `nix shell nixpkgs#<foo>`.
          nix.registry = { this.flake = inputs.nixpkgs; };
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
