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
    home = {
      packages =
        let
          myEmacs = pkgs.emacsWithPackagesFromUsePackage {
            config = ./emacs/init.el;
            alwaysEnsure = true;
            defaultInitFile = true;
            package = pkgs.emacs-unstable;
            # https://discourse.nixos.org/t/tree-sitter-grammars-collide-with-each-other/41805/7
            extraEmacsPackages = epkgs: [
              epkgs.tree-sitter
              epkgs.tree-sitter-langs
              (epkgs.treesit-grammars.with-grammars (
                grammars: with grammars; [
                  tree-sitter-html
                  tree-sitter-javascript
                  tree-sitter-tsx
                  tree-sitter-typescript
                ]
              ))
            ];

          };
        in
        [ myEmacs ];
    };
  };
}
