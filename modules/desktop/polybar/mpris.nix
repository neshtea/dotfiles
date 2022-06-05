{ pkgs, ... }:

let pctl = "${pkgs.playerctl}/bin/playerctl";
in pkgs.writeShellScriptBin "mpris" ''
  echo $(${pctl} --player=spotify metadata --format ' {{artist}} - {{title}} ')
''
