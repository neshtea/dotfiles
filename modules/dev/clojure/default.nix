{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.dev.clojure;
in
{
  options.modules.dev.clojure = {
    enable = lib.mkEnableOption "clojure";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # Clojure
      clojure
      clojure-lsp
      leiningen

      # ClojureScript
      nodejs

      # Java
      zulu
    ];
  };
}
