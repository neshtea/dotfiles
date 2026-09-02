{
  config,
  pkgs,
  lib,
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
    xdg.configFile = {
      # Only link what I actually need,
      "emacs/init.el".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/modules/programs/emacs/emacs/init.el";
      "emacs/early-init.el".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/modules/programs/emacs/emacs/early-init.el";
    };
    home = {
      packages =
        let
          emacsWithPackages = (pkgs.emacsPackagesFor cfg.emacsPackage).emacsWithPackages (p: [
            p.treesit-grammars.with-all-grammars
            p.erlang
          ]);
        in
        [
          emacsWithPackages
          pkgs.typescript
          pkgs.claude-agent-acp
          pkgs.zig # for ghostel
        ];
    };
  };
}
