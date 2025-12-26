{theme, ...}: let
  c = theme.colors;
in {
  catppuccin.waybar.enable = true;
  programs.waybar = {
    enable = true;
    settings = {
      main = {
        layer = "bottom";
        modules-left = ["niri/workspaces"];
        modules-center = ["clock"];
        modules-right = ["battery" "cpu" "memory" "tray"];

        clock.format = " {:%a %b %d | Week %V   %H:%M:%S}";
        clock.interval = 1;
        tray.spacing = 8;
        battery = {
          states = {
            good = 80;
            warning = 30;
            critical = 20;
          };
          interval = 1;
          tooltip = true;
          format = "{icon} {capacity}%";
          format-chargin = "<b>{icon}󱐋 {capacity}%</b>";
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
      };
    };

    style = ''
      window#waybar {
        padding: 0px 10px;
        margin: 0px 15px;
        background: ${c.base};
        color: ${c.text};
      }
      @import "./style2.css";
    '';
  };
}
