{
  config,
  pkgs,
  lib,
  # inputs,
  ...
}:

let
  cfg = config.modules.programs.ghostty;
in

{
  options.modules.programs.ghostty = {
    enable = lib.mkEnableOption "ghostty";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.jetbrains-mono ];
    programs = {
      ghostty = {
        enable = true;
        enableFishIntegration = true;
        package = if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
        settings = {
          command = lib.getExe config.programs.fish.package;
          font-family = "JetBrains Mono";
          font-size = 12;
          term = "xterm-256color";
          theme = "Gruvbox Dark Hard";
        };
      };
    };
  };
}
