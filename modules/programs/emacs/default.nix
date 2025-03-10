{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:

let
  cfg = config.modules.programs.emacs;
in
{
  options.modules.programs.emacs = {
    enable = lib.mkEnableOption "emacs";
    emacsPackage = lib.mkOption {
      type = lib.types.package;
      example = lib.literalExpression "pkgs.emacsMacport";
      description = "The emacs package that should be used as a base for emacs.";
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -e $XDG_CONFIG_HOME/emacs ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/programs/emacs/emacs $XDG_CONFIG_HOME/emacs
          fi
        '';
      };

      packages =
        let
          emacsWithPackages = (pkgs.emacsPackagesFor cfg.emacsPackage).emacsWithPackages (p: [
            p.vterm
            p.treesit-grammars.with-all-grammars
          ]);
        in
        [
          emacsWithPackages
          pkgs.texliveMedium # for org pdf export
        ];
    };
  };
}
