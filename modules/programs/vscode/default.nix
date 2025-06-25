{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.programs.vscode;
in
{
  options.modules.programs.vscode = {
    enable = lib.mkEnableOption "vscode";
  };
  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      userSettings = {
        "calva.useTestExplorer" = true;
        "editor.minimap.enabled" = false;
        "haskell.formattingProvider" = "fourmolu";
        "haskell.manageHLS" = "PATH";
        "window.autoDetectColorScheme" = true;
        "workbench.iconTheme" = "catppuccin-latte";
        "workbench.preferredDarkColorTheme" = "Catppuccin Macchiato";
        "workbench.preferredLightColorTheme" = "Catppuccin Latte";
        "workbench.sideBar.location" = "right";
      };
      extensions = with pkgs.vscode-marketplace; [
        alefragnani.project-manager
        betterthantomorrow.calva
        catppuccin.catppuccin-vsc
        catppuccin.catppuccin-vsc-icons
        elixir-lsp.elixir-ls
        erlang-ls.erlang-ls
        haskell.haskell
        jnoortheen.nix-ide
        mhutchie.git-graph
        mkhl.direnv
        ms-azuretools.vscode-containers
        ms-azuretools.vscode-docker
        mtxr.sqltools
        mtxr.sqltools-driver-pg
        scalameta.metals
        sumneko.lua
      ];
    };
  };
}
