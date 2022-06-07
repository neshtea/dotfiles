{ pkgs, parentBar, ... }:
let
  getSpotifyStatus =
    import ./get-spotify-status.nix { inherit pkgs parentBar; };
in pkgs.writeShellScriptBin "scroll-spotify-status" ''
  # see man zscroll for documentation of the following parameters
  ${pkgs.zscroll}/bin/zscroll -l 30 \
          --delay 0.1 \
          --scroll-padding "  " \
          --match-command "${getSpotifyStatus}/bin/get-spotify-status --status" \
          --match-text "Playing" "--scroll 1" \
          --match-text "Paused" "--scroll 0" \
          --update-check true "${getSpotifyStatus}/bin/get-spotify-status" &

  wait
''
