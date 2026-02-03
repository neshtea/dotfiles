{ pkgs, inputs, ... }:
{
  imports = [
    ../common.nix
  ];

  home.packages =
    let
      desktopPackages = import ../desktop.nix { inherit pkgs; };
    in
    desktopPackages
    ++ [
      pkgs.signal-desktop
      pkgs.jetbrains-mono
      pkgs.gcc
      pkgs.mattermost-desktop
      pkgs.thunderbird
      inputs.rose-pine-hyprcursor.packages.${pkgs.system}.default
      inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
      pkgs.bitwarden-desktop
    ];

  programs = {
    firefox.enable = true;
    lazygit.enable = true;
  };

  modules = {
    programs = {
      ghostty.enable = true;
      emacs = {
        enable = false;
        emacsPackage = pkgs.emacs-unstable;
      };
    };
    desktop = {
      hyprland.enable = true;
    };
  };
  dconf.enable = true;
}
