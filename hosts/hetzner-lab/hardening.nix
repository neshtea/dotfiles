{ ... }:
{
  services.fail2ban = {
    enable = true;
    maxretry = 5;
    bantime = "1h";
    bantime-increment = {
      enable = true; # repeat offenders get progressively longer bans
      maxtime = "168h";
    };
    jails = {
      # Vaultwarden logs failed master-password attempts; this filter
      # matches its "Username or password is incorrect" log line.
      vaultwarden.settings = {
        enabled = true;
        filter = "vaultwarden";
        action = "iptables-allports[name=vaultwarden]";
        logpath = "/var/lib/vaultwarden/vaultwarden.log";
        maxretry = 5;
        bantime = "1h";
      };
      caddy-abuse.settings = {
        enabled = true;
        filter = "caddy-abuse";
        action = "iptables-allports[name=caddy]";
        logpath = "/var/log/caddy/access-*.log";
        maxretry = 20;
        findtime = "10m";
      };
    };
  };
  environment.etc."fail2ban/filter.d/vaultwarden.conf".text = ''
    [Definition]
    failregex = ^.*Username or password is incorrect\. Try again\. IP: <HOST>\..*$
    ignoreregex =
  '';

  # Caddy access-log jail: bans IPs hammering any reverse-proxied vhost
  # with 401/403/404s (typical scanner/bruteforce behavior).
  services.caddy.logDir = "/var/log/caddy";
  environment.etc."fail2ban/filter.d/caddy-abuse.conf".text = ''
    [Definition]
    failregex = ^.*"remote_ip":"<HOST>".*"status":(401|403|404).*$
    ignoreregex =
  '';

  # Hardening
  services.journald.storage = "persistent";
  services.journald.extraConfig = "SystemMaxUse=500M";
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = false;
    "net.ipv4.conf.all.accept_redirects" = false;
    "net.ipv4.conf.all.accept_source_route" = false;
    "net.ipv4.tcp_syncookies" = true;
    "net.ipv6.conf.all.accept_redirects" = false;
  };
}
