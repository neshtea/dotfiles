{
  config,
  inputs,
  lib,
  pkgs,
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

      activation = {
        symlinkHyprland = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -e $XDG_CONFIG_HOME/hypr/hyprland.conf ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/desktop/hyprland/hypr/hyprland.conf $XDG_CONFIG_HOME/hypr/hyprland.conf
          fi
        '';
      };

      packages = [
        pkgs.hyprpaper
        pkgs.ashell
        pkgs.kdePackages.dolphin # graphical file browser
        pkgs.kdePackages.gwenview # image viewer
        pkgs.kdePackages.okular # pdf viewer
        pkgs.pavucontrol
        pkgs.networkmanager
        pkgs.networkmanagerapplet
      ];
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = "${pkgs.kdePackages.okular}/bin/okular";
        "image/jpeg" = "${pkgs.kdePackages.gwenview}/bin/gwenview";
        "image/png" = "${pkgs.kdePackages.gwenview}/bin/gwenview";
      };
    };

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
      waybar = {
        enable = true;
        settings = {
          mainBar = {
            layer = "top";
            position = "top";
            height = 34;
            spacing = 4;

            modules-left = [ "hyprland/workspaces" ];
            modules-center = [ "clock" ];
            modules-right = [
              "tray"
              "bluetooth"
              "network"
              "wireplumber"
              "battery"
            ];

            "hyprland/workspaces" = {
              format = "{name}";
              on-click = "activate";
              sort-by-number = true;
            };

            clock = {
              format = "{:%a %b %d  %I:%M %p}";
              tooltip-format = "<tt><small>{calendar}</small></tt>";
              calendar = {
                mode = "month";
                format = {
                  months = "<span color='#d5c4a1'><b>{}</b></span>";
                  days = "<span color='#ebdbb2'>{}</span>";
                  weekdays = "<span color='#fabd2f'><b>{}</b></span>";
                  today = "<span color='#fb4934'><b><u>{}</u></b></span>";
                };
              };
            };

            tray = {
              icon-size = 16;
              spacing = 8;
            };

            bluetooth = {
              format = " {status}";
              format-connected = " {num_connections}";
              format-disabled = "";
              tooltip-format = "{controller_alias}\t{controller_address}";
              tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{device_enumerate}";
              tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
            };

            network = {
              format-wifi = "  {signalStrength}%";
              format-ethernet = " ";
              format-disconnected = "󰤮 ";
              tooltip-format = "{ifname}: {ipaddr}/{cidr}";
              tooltip-format-wifi = "{essid} ({signalStrength}%)\n{ipaddr}/{cidr}";
              tooltip-format-ethernet = "{ifname}\n{ipaddr}/{cidr}";
              on-click = "nm-connection-editor";
            };

            wireplumber = {
              format = "{icon} {volume}%";
              format-muted = " ";
              format-icons = [
                ""
                ""
                ""
              ];
              on-click = "pavucontrol";
              scroll-step = 1;
            };

            battery = {
              states = {
                warning = 30;
                critical = 15;
              };
              format = "{icon} {capacity}%";
              format-charging = "󰂄 {capacity}%";
              format-plugged = " {capacity}%";
              format-icons = [
                ""
                ""
                ""
                ""
                ""
              ];
            };
          };
        };

        style = ''
          * {
              font-family: "JetBrains Mono", monospace;
              font-size: 13px;
              font-weight: 500;
          }

          window#waybar {
              background-color: transparent;
              color: #ebdbb2;
          }

          /* Workspaces */
          #workspaces {
              background-color: transparent;
              margin: 0;
              padding: 0;
          }

          #workspaces button {
              padding: 0 10px;
              background-color: transparent;
              color: #a89984;
              border: none;
              border-radius: 0;
          }

          #workspaces button:hover {
              background-color: rgba(168, 153, 132, 0.2);
              color: #ebdbb2;
          }

          #workspaces button.active {
              background-color: transparent;
              color: #fabd2f;
              font-weight: bold;
          }

          #workspaces button.urgent {
              background-color: rgba(251, 73, 52, 0.3);
              color: #fb4934;
          }

          /* Clock */
          #clock {
              background-color: transparent;
              color: #ebdbb2;
              padding: 0 15px;
          }

          /* Right modules - common styling */
          #tray,
          #bluetooth,
          #network,
          #wireplumber,
          #battery {
              background-color: transparent;
              color: #ebdbb2;
              padding: 0 10px;
          }

          /* Tray */
          #tray > .passive {
              -gtk-icon-effect: dim;
          }

          #tray > .needs-attention {
              -gtk-icon-effect: highlight;
              color: #fb4934;
          }

          /* Bluetooth */
          #bluetooth {
              color: #83a598;
          }

          #bluetooth.disabled {
              color: #665c54;
          }

          #bluetooth.connected {
              color: #8ec07c;
          }

          /* Network */
          #network {
              color: #b8bb26;
          }

          #network.disconnected {
              color: #fb4934;
          }

          /* Wireplumber */
          #wireplumber {
              color: #fabd2f;
          }

          #wireplumber.muted {
              color: #fb4934;
          }

          /* Battery */
          #battery {
              color: #8ec07c;
          }

          #battery.charging {
              color: #b8bb26;
          }

          #battery.warning:not(.charging) {
              color: #fabd2f;
          }

          #battery.critical:not(.charging) {
              color: #fb4934;
              animation: blink 0.5s linear infinite;
          }

          @keyframes blink {
              to {
                  color: #cc241d;
              }
          }
        '';
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
