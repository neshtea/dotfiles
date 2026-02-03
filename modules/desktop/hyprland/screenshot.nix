{ pkgs }:

pkgs.writeShellScriptBin "screenshot" ''
  ${pkgs.grim}/bin/grim -g "Screenshot" - | convert - -shave 2x2 PNG:- | ${pkgs.wl-clipboard}/bin/wl-copy
  ${pkgs.dunst}/bin/dunstify -t 3000 -u low -a screenshot "Screenshot copied to clipboard"
  ${pkgs.wl-clipboard}/bin/wl-paste | ${pkgs.swappy}/bin/swappy -f -
''
