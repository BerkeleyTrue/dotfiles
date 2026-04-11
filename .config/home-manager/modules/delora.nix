{
  inputs,
  pkgs,
  ...
}: let
  mkMonitor = inputs.self.monitor_utils.mkMonitor;
  centerSelfOnBase = inputs.self.monitor_utils.mkMonitor;
in {
  # main workstation
  flake.modules.homeManager.delora = {
    targets.genericLinux.nixGL.defaultWrapper = "mesa";

    home.packages = with pkgs; [
      nvtopPackages.amd
    ];

    # deprecated
    nixGLPackage = "mesa";
  };

  flake.modules.hardware.delora = let
    g5 = mkMonitor {
      height = 1440;
      width = 3440;
      label = "HDMI-A-1";
      rate = 165;
      scale = 1.25;
      position = {
        x = 0;
        y = dell.logical.height;
      };
    };
    dell = mkMonitor {
      height = 1080;
      width = 2560;
      label = "DP-3";
      rate = 60;
      scale = 1.25;
      position = centerSelfOnBase g5 dell;
    };
  in {
    monitors = {
      inherit dell g5;
    };
  };
}
