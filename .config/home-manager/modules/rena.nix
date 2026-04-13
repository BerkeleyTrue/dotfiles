{
  pkgs,
  self,
  ...
}: let
  mkMonitor = self.monitor_utils.mkMonitor;
in {
  # main workstation
  flake.modules.homeManager.rena = {
    targets.genericLinux.nixGL.defaultWrapper = "mesaPrime";

    home.packages = with pkgs; [
      nvtopPackages.intel
    ];
  };

  flake.modules.hardware.delora.monitors = {
    framework = mkMonitor {
      height = 1504;
      width = 2256;
      label = "eDP-1";
      rate = 60;
      scale = 1.5;
    };
  };
}
