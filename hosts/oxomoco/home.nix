{ pkgs, inputs, ... }:
{
  imports = [
    ../common.nix
  ];

  home.packages =
    let
      desktopPackages = import ../desktop.nix { inherit pkgs; };
      fonts = with pkgs.nerd-fonts; [
        comic-shanns-mono
        jetbrains-mono
        victor-mono
      ];
    in
    desktopPackages
    ++ [
      pkgs.signal-desktop
      pkgs.gcc
      pkgs.mattermost-desktop
      pkgs.thunderbird
      pkgs.bitwarden-desktop
    ]
    ++ fonts;

  programs = {
    firefox.enable = true;
    lazygit.enable = true;
  };

  modules = {
    programs = {
      ghostty.enable = true;
      emacs = {
        enable = true;
        emacsPackage = pkgs.emacs;
      };
    };
    desktop = {
      hyprland.enable = false;
    };
  };

  services = {
    syncthing = {
      enable = true;
    };
  };
}
