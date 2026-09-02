{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.dev.nix;
in
{
  options.modules.dev.nix = {
    enable = lib.mkEnableOption "nix";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nixfmt
      nil
    ];
  };
}
