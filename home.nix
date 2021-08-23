{ config, pkgs, ... }:
rec
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "schneider";
  home.homeDirectory = "/Users/${home.username}";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  imports = [./modules];

  home.packages = with pkgs; [
    cacert
    clojure  # for compiling/running clojure code
    clojure-lsp
    clj-kondo  # for static clojure code checking
    coreutils
    docker
    docker-compose
    ffmpeg
    fzf
    git
    gnugrep
    hledger
    leiningen
    multimarkdown
    msmtp  # for sending email
    nodejs
    notmuch  # for reading email
    ripgrep
    rlwrap
    ruby
    silver-searcher
    trash-cli
    vagrant  # for old factorylink-vpn
    wget
    youtube-dl
  ];

  accounts.email = {
    maildirBasePath = "${home.homeDirectory}/Mail";
    certificatesFile = "/etc/ssl/cert.pem";
    accounts = {
      # https://beb.ninja/post/email/
      posteo = {
        address = "marco.schneider@posteo.de";
        imap = {
          host = "posteo.de";
          port = 993;
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        # mu.enable = true;
        primary = true;
        realName = "Marco Schneider";
        passwordCommand = "pass show email/marco.schneider@posteo.de";
        smtp = {
          host = "posteo.de";
        };
        userName = "marco.schneider@posteo.de";
      };
      ag = {
        address = "marco.schneider@active-group.de";
        imap = {
          host = "imap.active-group.de";
          port = null;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        # mu.enable = true;
        realName = "Marco Schneider";
        passwordCommand = "pass show email/marco.schneider@active-group.de";
        smtp = {
          host = "smtp.active-group.de";
        };
        userName = "schneider";
      };
    };
  };

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "Marco Schneider";
    userEmail = "marco.schneider@active-group.de";
  };

  programs.mbsync.enable = true;

  programs.msmtp.enable = true;

  programs.mercurial = {
    enable = true;
    userName = "Marco Schneider";
    userEmail = "marco.schneider@active-group.de";
  };

  # https://beb.ninja/post/email/
  programs.notmuch = {
    enable = true;
    hooks = {
      # TODO Johannes fragen wie geht richtig
      preNew = "sh ${./script/notmuch/preNew.sh}";
      postNew = "sh ${./script/notmuch/postNew.sh}";
    };
  };

  programs.password-store.enable = true;

  # Custom modules.
  modules.editors.neovim.enable = true;
  modules.editors.emacs.enable = true;

  modules.programs.kitty.enable = true;

  modules.shell.fish.enable = true;
}
