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
      modules-left = ["niri/workspaces" "clock#date"];
      modules-center =
        lib.intersperse "custom/separator"
        ["custom/wttr" "clock" "custom/wakatime"];
      modules-right =
        (lib.intersperse "custom/separator"
          [
            "custom/powermenu"
            "cpu"
            "custom/cpu-temp"
            "memory"
            "custom/swaync"
            "custom/connectivity"
            "disk"
          ])
        ++ ["tray"];
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
        ["clock#date" "clock"];
      modules-right =
        lib.intersperse "custom/separator"
        ["custom/eth" "custom/btc"];
    }
    // common-modules;
}
