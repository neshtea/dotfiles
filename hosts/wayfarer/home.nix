{ config, pkgs, inputs, ... }: rec {
  imports = [ ../home.nix ];
  home.packages = with pkgs; [
    # macOS specific stuff
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.Security
    jre
  ];

  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacs-git;
    # emacsPackage = pkgs.emacsMacport;
  };
  # modules.programs.docker.enable = true;

  xdg.configFile."clj-kondo" = {
    source = "${inputs.active-kondo}";
    recursive = true;
  };
}
