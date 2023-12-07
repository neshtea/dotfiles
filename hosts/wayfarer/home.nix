{ config, pkgs, inputs, ... }: rec {
  imports = [ ../home.nix inputs.mac-app-util.homeManagerModules.default ];
  home.packages = with pkgs; [
    # macOS specific stuff
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.Security
    jre
  ];

  # Programs
  # modules.programs.neovim.enable = true;
  # modules.programs.kitty.enable = true;
  modules.programs = {
    wezterm = {
      enable = true;
      installPackage = false;
    };
  };

  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacs-git;
    # emacsPackage = pkgs.emacsMacport;
  };
  # modules.programs.docker.enable = true;

}
