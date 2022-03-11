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
  home.stateVersion = "21.05";

  imports = [ ../modules ];

  home.packages = with pkgs; [
    clojure # for compiling/running clojure code
    clj-kondo # for static clojure code checking
    clojure-lsp
    coreutils
    docker
    elixir
    elixir_ls
    ffmpeg
    gnugrep
    gnupg
    hledger
    hledger-web
    gnumake
    mattermost
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
      enableFishIntegration = true;
      # Makes nix-shells a LOT faster
      nix-direnv.enable = true;
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
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
  modules.programs.kitty.enable = true;

  # Shells and shell tools
  modules.shell.fish.enable = true;
  modules.shell.tmux.enable = true;
}
