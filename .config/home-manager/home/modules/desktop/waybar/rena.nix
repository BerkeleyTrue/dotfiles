{
  lib,
  common-modules,
  ...
}: {
  renaTop =
    {
      name = "rena-top";
      layer = "bottom";
      modules-left = ["niri/workspaces"];
      modules-center =
        lib.intersperse "custom/separator"
        ["custom/wttr" "clock" "custom/wakatime"];
      modules-right =
        lib.intersperse "custom/separator"
        ["custom/cpu-temp" "disk" "cpu" "memory" "tray"];

      battery = {
        states = {
          good = 100;
          warning = 30;
          danger = 20;
        };
        interval = 1;
        tooltip = true;
        format = "{icon} {capacity}%";
        format-charging = "<b>󰂄 {capacity}%</b>";
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
    }
    ++ common-modules;
  bottom =
    {
      name = "rena-bottom";
      layer = "bottom";
      modules-left = ["clock#date"];
      modules-center =
        lib.intersperse "custom/separator"
        ["custom/wakatime" "custom/eth" "custom/btc"];
      modules-right =
        lib.intersperse "custom/separator"
        ["battery" "custom/connectivity"];
    }
    ++ common-modules;
}
