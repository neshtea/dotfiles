{ config, pkgs, ... }: rec {
  imports = [ ./home.nix ];

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [ firefox element mattermost-desktop iosevka ];

  modules.programs.kitty.enable = true;
}
