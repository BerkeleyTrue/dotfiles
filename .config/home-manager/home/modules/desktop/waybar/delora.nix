{
  lib,
  common-modules,
  monitors,
  ...
}: {
  delora-primary =
    {
      name = "delora-primary";
      layer = "bottom";
      height = 34;
      output = monitors.g5.label;
      modules-left = ["niri/workspaces" "custom/separator" "clock#date"];
      modules-center =
        lib.intersperse "custom/separator"
        ["clock" "custom/wakatime" "custom/eth"];
      modules-right =
        lib.intersperse "custom/separator"
        ["custom/powermenu" "custom/connectivity" "custom/cpu-temp" "disk" "cpu" "memory" "tray"];
    }
    // common-modules;
  delora-secondary =
    {
      name = "delora-secondary";
      layer = "bottom";
      height = 34;
      output = monitors.dell.label;
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
