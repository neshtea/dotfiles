{
  config,
  pkgs,
  inputs,
  ...
}:
rec {
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

  nix.registry = {
    this.flake = inputs.nixpkgs;
  };

  home.username = "schneider";

  home.packages = with pkgs; [
    cacert
    coreutils

    tree-sitter
    # we have lots or clojure projects that don't provide a nix shell,
    # so let's have this available everywhere.
    clojure
    clojure-lsp # Not every project brings it's own lsp.
    rust-analyzer
    lua-language-server
    nil
    nixd
    leiningen
    neovim

    # most projects don't define a specific docker/docker-compose, so
    # let's have this available user-wide.
    docker
    docker-compose

    # tree-sitter
    (tree-sitter.withPlugins (p: [
      p.tree-sitter-clojure
      p.tree-sitter-elisp
      p.tree-sitter-ruby
      p.tree-sitter-ocaml
      p.tree-sitter-ocaml-interface
      p.tree-sitter-nix
      p.tree-sitter-haskell
      p.tree-sitter-make
      p.tree-sitter-markdown-inline
    ]))

    # Same here: Most project that are managed with make assume you
    # have it already.
    gnumake

    # Some system stuff that is independent of coding/projects
    ffmpeg
    gnugrep
    gnupg
    imagemagick
    rlwrap
    tree
    yt-dlp
    wget
    jq

    subversion

    # Required for some modules and nice utilities to have
    nixfmt-rfc-style
    multimarkdown
    ripgrep
    fd
    nodejs
    perl
    zulu17
  ];

  programs = {
    bat.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
      # Makes nix-shells a LOT faster
      nix-direnv.enable = true;
    };

    git = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
      delta.enable = true;
      extraConfig = {
        commit = {
          gpgsign = "true";
        };
        tag = {
          gpgsign = "true";
        };
        gpg = {
          format = "ssh";
          ssh.allowedSignersFile = "~/.ssh/allowed_signers";
        };
        init.defaultBranch = "main";
        merge.conflicstyle = "diff3";
        pull.rebase = "true";
        push.autoSetupRemote = "true";
        url = {
          # kenranunderscore forces me to use this as well... :D
          "https://github.com/" = {
            insteadOf = "gh:";
          };
          "ssh://git@gitlab.active-group.de:1022/ag/" = {
            insteadOf = "ag:";
          };
        };
      };
      ignores = [
        # (n)vim
        "*.swp"
        ".exrc"
        ".nvimrc"

        # Direnv
        ".direnv/"
        ".envrc"

        # macOS
        ".DS_Store"

        # Emacs: backup, auto-save, lock files, directory-local
        # variables
        "*~"
        "\\#*\\#"
        ".\\#*"
        ".dir-locals.el"

        # Clojure, LSP, ...
        ".clj-kondo/"
        ".lsp/"
        ".calva/"
        ".shadow-cljs/"
        ".cpcache/"

        # JavaScript
        "node_modules/"
      ];
      signing = {
        signByDefault = true;
        key = "~/.ssh/id_rsa.pub";
      };
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
  };

  # Shells and shell tools
  modules.shell.fzf.enable = true;
  modules.shell.tmux.enable = true;
  modules.shell.zsh.enable = true;

  xdg.enable = true;

  xdg.configFile."nixpkgs/config.nix".source = ../xdg/config.nix;
}
