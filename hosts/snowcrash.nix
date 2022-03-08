{ ... }: rec {

  imports = [ ./common.nix ];

  home.username = "schneider";
  home.homeDirectory = "/home/${home.username}";
}
