{ config, lib, ... }:

let
  cfg = config.modules.programs.zsh;
in
{
  options.modules.programs.zsh = {
    enable = lib.mkEnableOption "zsh";
  };
  config = lib.mkIf cfg.enable {
    programs = {
      zsh = {
        enable = true;
        defaultKeymap = "emacs";
        initExtra = ''
          export PATH="$HOME/bin:$PATH"
          [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"
          if [[ "$TERM" == "eat-truecolor" ]]; then
            export TERMINFO="$EAT_SHELL_INTEGRATION_DIR/../terminfo"
          fi
        '';
        initExtraBeforeCompInit = ''
          if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
            . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
          fi
        '';
      };
    };
  };
}
