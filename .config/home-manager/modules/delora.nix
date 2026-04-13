{self, ...}: let
  inherit (self.modules) homeManager;
  mkMonitor = self.monitor_utils.mkMonitor;
  centerSelfOnBase = self.monitor_utils.mkMonitor;
  username = "berkeleytrue";
in {
  # main workstation
  flake.modules.homeManager.delora = {pkgs, ...}: {
    targets.genericLinux.nixGL.defaultWrapper = "mesa";

    home.packages = with pkgs; [
      nvtopPackages.amd
    ];

    # see ./nixgl.nix
    nixgl.package = "mesa";
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

  configurations.home.delora = {
    inherit username;
    system = "x86_64-linux";
    modules = with homeManager; [
      nixgl
      delora
    ];
  };
}
