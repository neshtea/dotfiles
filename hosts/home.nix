{ pkgs, inputs, ... }:
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

  nix.registry.this.flake = inputs.nixpkgs;

  home.username = "schneider";

  home.sessionVariables = {
    EDITOR = "nvim";
    ALTERNATE_EDITOR = "vim";
  };

  home.packages = with pkgs; [
    cacert
    clojure
    coreutils
    docker-client
    gnumake
    iosevka
    jq
    multimarkdown
    nil
    nixVersions.latest
    nixd
    nixfmt-rfc-style
    nodejs
    ripgrep
    subversion
    tree-sitter
    wget
    lazygit
  ];

  programs = {

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

    direnv = {
      enable = true;
      enableZshIntegration = true;
      # Makes nix-shells a LOT faster
      nix-direnv.enable = true;
    };

    git = {
      delta.enable = true;
      enable = true;
      extraConfig = {
        commit.gpgsign = "true";
        core.editor = "nvim";
        gpg = {
          format = "ssh";
          ssh.allowedSignersFile = "~/.ssh/allowed_signers";
        };
        init.defaultBranch = "main";
        merge.conflicstyle = "diff3";
        pull.rebase = "true";
        push.autoSetupRemote = "true";
        tag.gpgsign = "true";
      };
      ignores = [
        "*.swp"
        ".exrc"
        ".nvimrc"
        ".direnv/"
        ".envrc"
        ".DS_Store"
        "*~"
        "\\#*\\#"
        ".\\#*"
        ".dir-locals.el"
        ".clj-kondo/"
        ".lsp/"
        ".calva/"
        ".shadow-cljs/"
        ".cpcache/"
        ".vscode/"
        "node_modules/"
      ];
      signing = {
        signByDefault = true;
        key = "~/.ssh/id_rsa.pub";
      };
      userEmail = "marco.schneider@active-group.de";
      userName = "Marco Schneider";
    };

    mercurial = {
      enable = true;
      userName = "Marco Schneider";
      userEmail = "marco.schneider@active-group.de";
    };
  };

  # Shells and shell tools
  modules.shell.fzf.enable = false;
  modules.shell.tmux.enable = true;
  modules.shell.zsh.enable = true;
  modules.programs.neovim.enable = true;
  modules.programs.emacs = {
    enable = true;
    emacsPackage = pkgs.emacs-git;
  };
  modules.programs.wezterm.enable = false;

  xdg.enable = true;
  xdg.configFile."nixpkgs/config.nix".source = ../xdg/config.nix;
}
