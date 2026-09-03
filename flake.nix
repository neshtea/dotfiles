{
  description = "My NixOS and home-manager configuration as a flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-starter-kit = {
      url = "github:active-group/nix-starter-kit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      nixpkgs,
      home-manager,
      sops-nix,
      ...
    }:
    let
      username = "schneider"; # conveniently, this is my username on all systems.
      # Put things in `specialArgs` that we might need for
      # configuration further down the configuration (for example in
      # hm modules).
      specialArgs = { inherit inputs; };

      mkHome =
        {
          system,
          user ? username,
          host,
          homeDirectory,
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
          extraSpecialArgs = specialArgs;
          modules = [
            ./hosts/${host}/home.nix
            {
              home = {
                username = user;
                inherit homeDirectory;
                stateVersion = "22.05";
              };
            }
          ];
        };
    in
    {
      homeConfigurations = {
        "${username}@wayfarer" = mkHome {
          system = "aarch64-darwin";
          host = "wayfarer";
          homeDirectory = "/Users/${username}";
        };

        "pi@marvin" = mkHome {
          system = "aarch64-linux";
          host = "marvin";
          homeDirectory = "/home/${username}";
        };
      };
      nixosConfigurations = {
        hetzner-lab =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
          in
          nixpkgs.lib.nixosSystem {
            inherit pkgs system specialArgs;
            modules = [
              ./hosts/hetzner-lab/configuration.nix
              sops-nix.nixosModules.sops
              home-manager.nixosModules.home-manager
              {
                home-manager = {
                  users."marco" = import ./hosts/hetzner-lab/home.nix;
                  useGlobalPkgs = true;
                  useUserPackages = false;
                  extraSpecialArgs = specialArgs;
                };
              }
            ];
          };
        oxomoco =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              config.allowUnfree = true; # sorry rms
              inherit system;
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
    };
}
