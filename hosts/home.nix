{
  config,
  pkgs,
  inputs,
  ...
}:
{
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

  home.sessionVariables = {
    EDITOR = "nvim";
    ALTERNATE_EDITOR = "vim";
  };

  home.packages = with pkgs; [
    nixVersions.latest

    cacert
    coreutils
    tree-sitter
    clojure
    nixd
    nil
    leiningen
    lazygit

    docker-client
    gnumake
    # Some system stuff that is independent of coding/projects
    ffmpeg
    imagemagick
    rlwrap
    tree
    yt-dlp
    wget
    jq
    hledger
    hledger-web

    subversion

    # Required for some modules and nice utilities to have
    nixfmt-rfc-style
    multimarkdown
    ripgrep
    fd
    nodejs
    zulu23
  ];

  programs = {
    direnv = {
      enable = true;
      # Makes nix-shells a LOT faster
      nix-direnv.enable = true;
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
        source ~/.nix-profile/etc/profile.d/nix.fish
      '';
      shellInit = ''
        fish_add_path --path "$HOME/bin:$PATH"
        export TEXINPUTS="$HOME/repos/ag/howto/tex:$TEXINPUTS"
      '';
    };

    git = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
      delta.enable = true;
      extraConfig = {
        core = {
          editor = "nvim";
        };
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

        ".vscode/"

        # JavaScript
        "node_modules/"
      ];
      signing = {
        signByDefault = true;
        key = "~/.ssh/id_rsa.pub";
      };
    };

    mercurial = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
    };
  };

  # Shells and shell tools
  modules.shell.fzf.enable = true;
  modules.shell.tmux.enable = true;
  modules.shell.zsh.enable = false;
  modules.programs.ghostty.enable = true;
  modules.programs.neovim.enable = true;
  modules.programs.emacs.enable = true;

  xdg.enable = true;

  xdg.configFile."nixpkgs/config.nix".source = ../xdg/config.nix;
}
