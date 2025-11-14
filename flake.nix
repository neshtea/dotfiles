{
  description = "My NixOS and home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
  };

  outputs =
    inputs@{ nixpkgs, home-manager, ... }:
    let
      username = "schneider"; # conveniently, this is my username on all systems.
      # Put things in `specialArgs` that we might need for
      # configuration further down the configuration (for example in
      # hm modules).
      specialArgs = { inherit inputs; };
      makeConfiguration =
        system: homeModule:
        let
          pkgs = import nixpkgs {
            config.allowUnfree = true; # Sorry rms
            inherit system;
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
            homeModule
            {
              home = {
                inherit username;
                homeDirectory =
                  if pkgs.lib.hasSuffix system "linux" then "/home/${username}" else "/Users/${username}";
                stateVersion = "22.05";
              };
            }
          ];
        };

    in
    {
      homeConfigurations."${username}@wayfarer" =
        makeConfiguration "aarch64-darwin" ./hosts/wayfarer/home.nix;

      homeConfigurations."${username}@oxomoco" =
        makeConfiguration "x86_64-linux" ./hosts/oxomoco/home.nix;
    };
}
