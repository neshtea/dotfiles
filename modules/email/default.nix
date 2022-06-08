{ config, lib, pkgs, ... }:

let
  realName = "Marco Schneider";
  cfg = config.modules.email;
in {
  options.modules.email = {
    enable = lib.mkEnableOption "email";
    maildir = lib.mkOption {
      type = lib.types.str;
      default = "mail";
    };

    certificatesFile = {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };

    primaryAccount = {
      type = lib.types.enum [ "ag" "posteo" ];
      default = "posteo";
    };

    config = lib.mkIf cfg.enable {
      accounts.email = {
        maildirBasePath = cfg.maildir;
        certificatesFile = cfg.certificatesFile;

        accounts = {
          posteo = rec {
            address = "marco.schneider@posteo.de";
            userName = "marco.schneider@posteo.de";
            primary = cfg.primaryAccount == "posteo";
            mbsync = {
              enable = true;
              create = "both";
              remove = "both";
              expunge = "both";
            };
            msmtp = {
              enable = true;
              extraConfig = { "syslog" = "LOG_USER" };
            };
            inherit realName;
            passwordCommand = "pass show posteo.de";
            imap = {
              host = "imap.posteo.de";
              port = 993;
              tls.enable = true;
            };
            smtp = {
              host = "smtp.posteo.de";
              port = 465;
              tls.enable = true;
            };
          };
        };
      };
    };
  };
}
