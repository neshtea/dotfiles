{ config, pkgs, ... }: rec {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  imports = [ ../modules ];

  home.username = "schneider";

  home.packages = with pkgs; [
    cacert
    coreutils

    # we have lots or clojure projects that don't provide a nix shell,
    # so let's have this available everywhere.
    clojure
    clojure-lsp # Not every project brings it's own lsp.
    leiningen

    # most projects don't define a specific docker/docker-compose, so
    # let's have this available user-wide.
    docker
    docker-compose

    gcc

    # Same here: Most project that are managed with make assume you
    # have it already.
    gnumake

    # Accounting
    hledger
    hledger-web

    # Some system stuff that is independent of coding/projects
    ffmpeg
    gnugrep
    gnupg
    imagemagick
    rlwrap
    silver-searcher
    tree
    youtube-dl
    wget

    # Required for some modules and nice utilities to have
    nixfmt
    multimarkdown
    ripgrep
  ];

  programs = {
    bat.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
      # Makes nix-shells a LOT faster
      nix-direnv.enable = true;
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
      delta.enable = true;
    };

    gpg = {
      enable = true;
      homedir = "${config.xdg.dataHome}/gnupg";
    };

    htop.enable = true;

    mercurial = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
    };

    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions
        (exts: [ exts.pass-otp exts.pass-import exts.pass-genphrase ]);
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
        PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
        PASSWORD_STORE_CLIP_TIME = "120";
      };
    };
  };

  # Programs
  modules.programs.neovim.enable = true;

  # Shells and shell tools
  modules.shell.zsh.enable = true;
  modules.shell.tmux.enable = true;

  xdg.enable = true;

  xdg.configFile."nixpkgs/config.nix".source = ../xdg/config.nix;
}
