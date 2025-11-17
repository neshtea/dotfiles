# Only contain "additional" configuration for hyprland. Hyprland itself is installed as a nixos module in /flake.nix.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.modules.desktop.hyprland;
in
{
  options.modules.desktop.hyprland = {
    enable = lib.mkEnableOption "hyprland";
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = [
        # We assume hyprland is configured in configuration.nix of the caller
        pkgs.hyprpaper
        pkgs.ashell
        pkgs.kdePackages.dolphin # graphical file browser
        pkgs.kdePackages.gwenview # image viewer
        pkgs.kdePackages.okular # pdf viewer
      ];
    };

    # xdg.mimeApps = {
    #   enable = true;
    #   defaultApplications = {
    #     "application/pdf" = "${pkgs.kdePackages.okular}/bin/okular";
    #     "application/jpeg" = "${pkgs.kdePackages.gwenview}/bin/gwenview";
    #     "application/png" = "${pkgs.kdePackages.gwenview}/bin/gwenview";
    #   };
    # };

    programs = {
      hyprlock.enable = true;
      fuzzel = {
        enable = true;
        settings = {
          main = {
            terminal = "${pkgs.ghostty}/bin/ghostty";
            layer = "overlay";
            font = "JetBrains Mono:size=10";
          };
          colors = {
            background = "282828ff";
            text = "ebdbb2ff";
            prompt = "a89984ff";
            placeholder = "928374ff";
            input = "ebdbb2ff";
            match = "fabd2fef";
            selection = "665c54ff";
            selection-text = "ebdbb2ff";
            selection-match = "fabd2fef";
            counter = "928374ff";
            border = "fe8019ff";
          };
        };
      };
    };

    services = {
      dunst.enable = true;
      hypridle = {
        enable = true;
        # Settings taken from https://wiki.hypr.land/Hypr-Ecosystem/hypridle/
        settings = {
          general = {
            lock_cmd = "pidof hyprlock || hyprlock";
            before_sleep_cmd = "loginctl lock-session";
            after_sleep_cmd = "hyprctl dispatch dpms on";
          };
          listener = [
            # Dim backlight after 2,5 minutes.
            {
              timeout = 150;
              on-timeout = "brightnessctl -s set 10";
              on-resume = "brightnessctl -r";
            }
            # Lock session after 5 minutes.
            {
              timeout = 300;
              on-timeout = "loginctl lock-session";
            }
            # Turn off screen after 5,5 minutes.
            {
              timeout = 330;
              on-timeout = "hyprctl dispatch dpms off";
              on-resume = "hyprctl dispatch dpms on && brightnessctl -r";
            }
            # Suspend after 30 minutes.
            {
              timeout = 1800;
              on-timeout = "systemctl suspend";
            }
          ];
        };
      };
    };
  };
}
