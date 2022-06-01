{
  description = "My nix home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ nixpkgs, home-manager, ... }:
    let
      username = "schneider";
      # zeugs, das ich weiter "unten" in modulen vielleicht wissen
      # will.
      specialArgs = { inherit inputs; };
    in {

      # eine flake mit outputs fuer
      # - home-manager mit macos
      # - home-manager + configuration.nix + blablabla
      nixosConfigurations.anarres = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/anarres/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              users.${username} = import ./hosts/anarres/home.nix;
              # Johannes: Pkgs von globalem nix verwenden.  Aber:
              # globales nix ist ja genau das hier :)
              useGlobalPkgs = true;
              useUserPackages =
                # johannes weiss auch nicht genau, iwas mit
                # nix-env.  Steht in docstrings von
                # home-manager modul.
                false;
              extraSpecialArgs = specialArgs;
            };
          }
        ];
      };

      homeConfigurations.${username} = let
        system = "x86_64-darwin";
        pkgs = import nixpkgs {
          config.allowUnfree = true;
          inherit system;
        };
      in home-manager.lib.homeManagerConfiguration {
        # TODO
        configuration = import ./hosts/rombach/home.nix;

        inherit system username pkgs;
        homeDirectory = "/Users/schneider";
        stateVersion = "22.05";
      };
    };
}
