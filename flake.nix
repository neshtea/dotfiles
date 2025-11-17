{
  description = "My NixOS and home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs =
    inputs@{ nixpkgs, home-manager, ... }:
    let
      username = "schneider"; # conveniently, this is my username on all systems.
      # Put things in `specialArgs` that we might need for
      # configuration further down the configuration (for example in
      # hm modules).
      specialArgs = { inherit inputs; };
    in
    {
      homeConfigurations."${username}@wayfarer" =
        let
          pkgs = import nixpkgs {
            config.allowUnfree = true; # Sorry rms
            system = "aarch64-darwin";
            overlays = [
              inputs.emacs-overlay.overlays.default
              inputs.neovim-nightly-overlay.overlays.default
            ];
            # Use this flake.nix's nixpkgs for stuff like `nix shell nixpkgs#<foo>`.
            nix.registry = {
              this.flake = inputs.nixpkgs;
            };
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = [
            ./hosts/wayfarer/home.nix
            {
              home = {
                inherit username;
                homeDirectory = if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}";
                stateVersion = "22.05";
              };
            }
          ];
        };
      nixosConfigurations.oxomoco =
        let
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            config.allowUnfree = true; # sorry rms
            inherit system;
            overlays = [
              inputs.emacs-overlay.overlays.default
              inputs.neovim-nightly-overlay.overlays.default
            ];
          };
        in
        nixpkgs.lib.nixosSystem {
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
    };
}
