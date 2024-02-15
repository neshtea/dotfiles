{ config, pkgs, lib, ... }:
let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim = { enable = lib.mkEnableOption "neovim"; };

  # make config only if someone set enable = true
  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        neovim
        nodejs
        lua
        stylua
        jq # Formatter for json
        python310Packages.mdformat # Formatter for markdown
      ];
    };
  };
}
