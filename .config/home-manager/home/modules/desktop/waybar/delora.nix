{
  lib,
  common-modules,
  ...
}: {
  delora-top =
    {
      name = "delora-top";
      layer = "bottom";
      modules-left = ["niri/workspaces" "custom/separator" "clock#date"];
      modules-center =
        lib.intersperse "custom/separator"
        ["clock" "custom/wakatime" "custom/eth"];
      modules-right =
        lib.intersperse "custom/separator"
        ["custom/powermenu" "custom/connectivity" "custom/cpu-temp" "disk" "cpu" "memory" "tray"];
    }
    // common-modules;
  delora-bottom =
    {
      name = "delora-bottom";
      layer = "bottom";
      modules-left = ["niri/workspaces"];
      modules-center =
        lib.intersperse "custom/separator"
        ["custom/wttrbar" "clock#date" "clock"];
      modules-right =
        lib.intersperse "custom/separator"
        ["custom/btc"];
    }
    // common-modules;
}
