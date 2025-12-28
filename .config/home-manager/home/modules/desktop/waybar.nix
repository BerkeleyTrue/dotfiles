{lib, ...}: {
  programs.waybar = {
    enable = true;
    settings = {
      main = {
        layer = "bottom";
        modules-left = ["niri/workspaces"];
        modules-center = ["clock#clock2" "custom/separator" "clock" "custom/separator" "custom/wakatime"];
        modules-right = lib.intersperse "custom/separator" ["cpu" "temperature" "battery" "memory" "disk" "tray"];

        "clock#clock2" = {
          format = " {:%a %b %d}";
          interval = 60;
        };
        clock.format = "Week {:%V  %H:%M:%S}";
        clock.interval = 1;

        tray.spacing = 4;

        battery = {
          states = {
            good = 100;
            warning = 30;
            danger = 20;
          };
          interval = 1;
          tooltip = true;
          format = "{icon}{capacity}%";
          format-charging = "<b>󰂄{capacity}%</b>";
          format-icons = [
            "󰁺"
            "󰁻"
            "󰁼"
            "󰁽"
            "󰁾"
            "󰁿"
            "󰂀"
            "󰂁"
            "󰂂"
            "󰁹"
          ];
        };

        cpu = {
          interval = 1;
          format = " {usage}%";
          states = {
            danger = 90;
            warning = 75;
          };
        };

        temperature = {
          interval = 1;
          thermal-zone = 4;
          warning-threshold = 75;
          critical-threshold = 85;
          format = "{icon}{temperatureC}°C";
          format-icons = [
            ""
            ""
            ""
          ];
        };

        memory = {
          interval = 1;
          format = "{percentage}%";
          states = {
            danger = 90;
            warning = 75;
          };
        };

        disk = {
          interval = 60;
          # use letter spacing here to get some distance between icon and number
          format = "󰉉 {specific_free:.0f}G";
          unit = "GB";
        };

        "niri/workspaces" = {
          format = "{icon}";
          format-icons = {
            "1" = "󰇊";
            "2" = "󰇋";
            "3" = "󰇌";
            "4" = "󰇍";
            "5" = "󰇎";
            "6" = "󰇏";
          };
        };

        "custom/separator" = {
          format = "|";
        };

        "custom/wakatime" = {
          interval = 10;
          format = " {text}";
          exec = "wakatoday";
        };
      };
    };

    # use style file directly for faster iteration
    style = null;
  };
}
