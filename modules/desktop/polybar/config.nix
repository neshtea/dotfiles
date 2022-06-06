pkgs:

let
  colors = {
    foreground = "#fbf1c7";
    background = "#1d2021";
    transparent = "#00000000";
    urgent = "#ff4500";
  };

  mkBar = bottom: modules-left: modules-center: modules-right: {
    inherit bottom modules-left modules-center modules-right;
    monitor = "\${env:MONITOR:}";
    background = "${colors.background}";
    foreground = "${colors.foreground}";
    fixed-center = true;
    font-0 = "Iosevka Nerd Font:size=15;4";
    font-1 = "FiraCode Nerd Font:style=Bold:size=19;4";
    font-2 = "Iosevka Nerd Font:style=Bold:size=15;4";
    height = "30";
    locale = "en_US.UTF-8";
    offset-x = "0%";
    padding = "0";
    radius-top = "0";
    width = "100%";
    # display "above" window manager
    wm-restack = "generic";
    override-redirect = true;
    # tray-position = "left";
  };

  mkWlanModule = interface: {
    inherit interface;
    type = "internal/network";
    accumulate-stats = "true";
    format-connected = "<label-connected>";
    format-connected-background = "${colors.background}";
    format-connected-foreground = "${colors.foreground}";
    format-connected-margin = 0;
    format-connected-overline = "${colors.transparent}";
    format-connected-padding = 2;
    format-connected-underline = "${colors.transparent}";
    format-disconnected = "<label-disconnected>";
    format-disconnected-background = "${colors.background}";
    format-disconnected-foreground = "#909090";
    format-disconnected-margin = 0;
    format-disconnected-overline = "${colors.transparent}";
    format-disconnected-padding = 2;
    format-disconnected-underline = "${colors.transparent}";
    interval = "1.0";
    label-connected = "%essid% %signal%%";
    label-disconnected = "DISCONNECTED";
    unknown-as-up = true;
  };
in {
  "global/wm" = {
    margin-bottom = 0;
    margin-top = 0;
  };

  "bar/bottom" =
    mkBar false "mpris cpu memory filesystem" "xmonad" "wlan2 audio date";

  "module/date" = {
    format = "<label>";
    format-foreground = "${colors.foreground}";
    format-background = "${colors.background}";
    format-padding = 2;
    format-margin = 0;
    interval = 1;
    label = "%date% %time%";
    time = "%H:%M:%S";
    date = "%d %b %Y";
    type = "internal/date";
  };

  "module/cpu" = {
    format = "  <label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    interval = "0.5";
    label = "CPU %percentage%%";
    type = "internal/cpu";
  };

  "module/audio" = {
    format-muted = "<label-muted>";
    format-muted-background = "${colors.background}";
    format-muted-foreground = "${colors.urgent}";
    format-muted-overline = "${colors.transparent}";
    format-muted-padding = 2;
    format-muted-margin = 0;
    format-muted-prefix = "婢  ";
    format-muted-prefix-foreground = "${colors.urgent}";
    format-volume = "墳  VOL <label-volume>";
    format-volume-background = "${colors.background}";
    format-volume-foreground = "${colors.foreground}";
    format-volume-padding = 2;
    format-volume-margin = 0;
    label-muted = "MUTED";
    label-volume = "%percentage%%";
    type = "internal/pulseaudio";
  };

  "settings" = {
    compositing-background = "source";
    compositing-border = "over";
    compositing-foreground = "over";
    compositing-overline = "over";
    comppositing-underline = "over";
    pseudo-transparency = false;
    screenchange-reload = true;
    throttle-output = "5";
    throttle-output-for = "10";
  };

  "module/wlan1" = mkWlanModule "wlp3s0";
  "module/wlan2" = mkWlanModule "wlp4s0";

  "module/memory" = {
    format = "  <label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    format-margin = 0;
    interval = 3;
    label = "RAM %percentage_used%%";
    type = "internal/memory";
  };

  "module/xmonad" = {
    type = "custom/script";
    exec = "${pkgs.xmonad-log}/bin/xmonad-log";
    tail = true;
  };

  "module/mpris" = let mpris = import ./mpris.nix { inherit pkgs; };
  in {
    type = "custom/script";
    exec = "${mpris}/bin/mpris";
    tail = true;
    label-maxlen = 60;
    interval = 2;
    format = " <label>";
    format-padding = 2;
  };

  "module/filesystem" = {
    type = "internal/filesystem";
    format = "  <label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    format-margin = 0;
    mount-0 = "/";
    mount-1 = "/home";
  };
}
