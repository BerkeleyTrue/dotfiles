{
  pkgs,
  self,
  ...
}: let
  inherit (self) colors;
  mkMonitor = self.monitor_utils.mkMonitor;
in {
  # main workstation
  flake.modules.homeManager.rena = {
    targets.genericLinux.nixGL.defaultWrapper = "mesaPrime";

    home.packages = with pkgs; [
      nvtopPackages.intel
    ];

    monitors = {
      framework = {
        height = 1504;
        width = 2256;
        label = "eDP-1";
        rate = 60;
        scale = 1.5;
      };
    };

    wallpaper = {
      framework = {
        angle = 60;
        swirl = 360;
        gradient = {
          beginColor = colors.sapphire;
          endColor = colors.lavender;
        };
        logoColor = colors.text;
        height = 1504;
        width = 2256;
      };
    };
  };

  flake.modules.hardware.delora.monitors = {
  };
}
