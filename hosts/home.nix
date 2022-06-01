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

    clojure # for compiling/running clojure code
    clj-kondo # for static clojure code checking
    clojure-lsp
    # we have lots or clojure projects managed via leinigen
    # that don't provide a nix shell, so let's have this
    # available everywhere.
    leiningen

    coreutils

    docker
    docker-compose

    elixir
    elixir_ls

    tree
    ffmpeg
    imagemagick
    gnugrep
    gnupg
    gnumake
    hledger
    hledger-web
    multimarkdown
    nixfmt
    nodejs
    ripgrep
    rlwrap
    silver-searcher
    wget
    youtube-dl
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

    htop.enable = true;

    mercurial = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
    };
  };

  # Programs
  modules.programs.neovim.enable = true;
  modules.programs.emacs.enable = true;

  # Shells and shell tools
  modules.shell.zsh.enable = true;
  modules.shell.tmux.enable = true;

  xdg.configFile."nixpkgs/config.nix".source = ../config.nix;
}
