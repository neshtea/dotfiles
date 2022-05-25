{ config, pkgs, ... }: rec {
  imports = [ ./home.nix ];
  home.packages = with pkgs; [ firefox element mattermost-client ];

  modules.programs.kitty.enable = true;

}
