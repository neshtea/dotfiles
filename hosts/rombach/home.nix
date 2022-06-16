{ config, pkgs, ... }: rec {
  imports = [ ../home.nix ];
  home.packages = with pkgs; [
    # macOS specific stuff
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.Security
  ];

  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacsMacport;
  };
  # modules.programs.kitty.enable = true;
}
