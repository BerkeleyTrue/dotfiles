{
  self,
  ...
}: let
  inherit (self.modules) homeManager;
  inherit (self) colors;
  centerSelfOnBase = self.monitor_utils.centerSelfOnBase;
  username = "berkeleytrue";
in {
  # main workstation
  flake.modules.homeManager.delora = {
    config,
    pkgs,
    ...
  }: {
    targets.genericLinux.nixGL.defaultWrapper = "mesa";

    home.packages = with pkgs; [
      nvtopPackages.amd
    ];

    monitors = {
      g5 = {
        height = 1440;
        width = 3440;
        label = "HDMI-A-1";
        rate = 165;
        scale = 1.25;
        position = {
          x = 0;
          y = config.monitors.dell.logical.height;
        };
      };
      dell = {
        height = 1080;
        width = 2560;
        label = "DP-3";
        rate = 60;
        scale = 1.25;
        position = centerSelfOnBase config.monitors.g5 config.monitors.dell;
      };
    };

    wallpaper = {
      g5 = {
        angle = 30;
        gradient = {
          beginColor = colors.mauve;
          endColor = colors.sapphire;
        };
        logoColor = colors.overlay0;
        height = 1440;
        width = 3440;
      };

      dell = {
        angle = 30;
        gradient = {
          beginColor = colors.lavender;
          endColor = colors.sapphire;
        };
        logoColor = colors.subtext1;
        height = 1080;
        width = 2560;
      };
    };
  };

  configurations.home.delora = {
    inherit username;
    system = "x86_64-linux";
    modules = with homeManager; [
      nixgl
      monitor
      wallpaper
      delora
    ];
  };
}
