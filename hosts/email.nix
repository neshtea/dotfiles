{ pkgs, ... }: {
  config = {
    accounts.email = {
      maildirBasePath = "mail";
      certificatesFile = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      accounts = {
        ag = rec {
          address = "marco.schneider@active-group.de";
          userName = address;
          primary = true;
          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
            expunge = "both";
          };
          msmtp.enable = true;
          notmuch.enable = true;
          realName = "Marco Schneider";
          passwordCommand =
            "security find-generic-password -s mbsync-ag -a marco.schneider@active-group.de -w";
          imap = {
            host = "mail.active-group.de";
            port = 143;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };
          smtp = {
            host = "mail.active-group.de";
            port = 587;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };
        };
      };
    };

    programs = {
      mbsync.enable = true;
      msmtp.enable = true;
      notmuch = {
        enable = true;
        hooks.preNew = "mbsync -a";
      };
    };
  };
}
