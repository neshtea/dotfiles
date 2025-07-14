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
    ghostty = {
      url = "github:ghostty-org/ghostty";
    };
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
      # This only concerns systems that use home-manager directly (in
      # my case, darwin machines).
      homeConfigurations.${username} =
        let
          system = "aarch64-darwin"; # Only relevant for darwin.
          pkgs = import nixpkgs {
            config.allowUnfree = true; # Sorry rms
            inherit system;
            overlays = [
              inputs.emacs-overlay.overlays.default
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
                homeDirectory = "/Users/schneider";
                stateVersion = "22.05";
              };
            }
          ];
        };
    };
}
