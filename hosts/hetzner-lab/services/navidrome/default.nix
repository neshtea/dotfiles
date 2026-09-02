{
  config,
  smbDevice,
  smbCredentialsFile,
  ...
}:
{
  sops.secrets."navidrome/lastfm_secret" = { };
  fileSystems."/mnt/music" = {
    device = "${smbDevice}/Music";
    fsType = "cifs";
    options = [
      "credentials=${smbCredentialsFile}"
      "x-systemd.mount-timeout=30"
      "_netdev"
      "nofail"
      "uid=navidrome"
      "gid=navidrome"
      "file_mode=0640"
      "dir_mode=0750"
      "ro"
    ];
  };
  systemd.services.navidrome.unitConfig.RequiresMountsFor = "/mnt/music";
  services = {
    navidrome = {
      enable = true;
      settings = {
        Address = "127.0.0.1";
        Port = 4533;
        MusicFolder = "/mnt/music";
        ScanSchedule = "1h";
        LogLevel = "info";
        SessionTimeout = "24h";
        BaseUrl = "https://music.defmarco.com";
        LastFM.ApiKey = "42fc64a44dbe3fd134ab3aac391373e2"; # not secret, fine in nix store
        EnableTranscodingConfig = true;
      };
      environmentFile = config.sops.secrets."navidrome/lastfm_secret".path;
      # environmentFile must contain: ND_LASTFM_SECRET=...
    };
    caddy.virtualHosts."music.defmarco.com".extraConfig = ''
      reverse_proxy 127.0.0.1:4533
    '';

  };

}
