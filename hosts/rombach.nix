{ ... }: rec {

  imports = [ ./common.nix ];

  home.username = "schneider";
  home.homeDirectory = "/Users/${home.username}";
}
