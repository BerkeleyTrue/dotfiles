{
  lib,
  common-modules,
  ...
}: let
  height = 34;
in {
  renaTop =
    {
      inherit height;
      name = "rena-top";
      layer = "bottom";

      modules-left = ["niri/workspaces"];
      modules-center =
        lib.intersperse "custom/separator"
        ["custom/wttr" "clock"];
      modules-right =
        lib.intersperse "custom/separator"
        [
          "custom/powermenu"
          "cpu"
          "custom/cpu-temp"
          "memory"
        ]
        ++ ["tray"];
    }
    // common-modules;
  bottom =
    {
      inherit height;
      name = "rena-bottom";
      layer = "bottom";
      position = "bottom";

      modules-left = ["clock#date"];
      modules-center =
        lib.intersperse "custom/separator"
        ["custom/wakatime" "custom/eth" "custom/btc"];
      modules-right =
        lib.intersperse "custom/separator"
        ["custom/swaync" "battery" "custom/connectivity"];

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
    // common-modules;
}
