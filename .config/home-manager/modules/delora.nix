{self, ...}: let
  inherit (self.modules) homeManager;
  inherit (self) colors;
  centerSelfOnBase = self.monitor_utils.centerSelfOnBase;
  username = "berkeleytrue";
in {
  # main workstation
  flake.modules.homeManager.delora = {
    config,
    pkgs,
    lib,
    ...
  }: let
    monitors = config.monitors;
    commonModules = config.waybar.commonModules;
  in {
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
          y = monitors.dell.logical.height;
        };
      };
      dell = {
        height = 1080;
        width = 2560;
        label = "DP-3";
        rate = 60;
        scale = 1.25;
        position = centerSelfOnBase monitors.g5 monitors.dell;
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

    programs.waybar.settings = {
      delora-primary =
        {
          name = "delora-primary";
          layer = "bottom";
          height = 34;
          output = monitors.g5.label;
          modules-left = ["niri/workspaces" "clock#date"];
          modules-center =
            lib.intersperse "custom/separator"
            ["custom/wttr" "niri/window" "clock" "custom/wakatime"];
          modules-right =
            lib.intersperse "custom/separator"
            [
              "custom/powermenu"
              "cpu"
              "custom/cpu-temp"
              "memory"
              "custom/swaync"
              "pulseaudio"
              "custom/connectivity"
              "disk"
            ];
        }
        // commonModules;

      delora-secondary =
        {
          name = "delora-secondary";
          layer = "bottom";
          height = 34;
          output = monitors.dell.label;
          modules-left = ["niri/workspaces"];
          modules-center =
            lib.intersperse "custom/separator"
            ["clock#date" "niri/window" "clock"];
          modules-right =
            lib.intersperse "custom/separator"
            ["custom/eth" "custom/btc"];
        }
        // commonModules;
    };

    swayncOutput = monitors.g5.label;
  };

  configurations.home.delora = {
    inherit username;
    system = "x86_64-linux";
    modules = with homeManager; [
      awww
      blueman-applet
      catppuccin
      cli-tools
      delora
      desktop
      desktop-apps
      dev
      fonts
      gh-user
      hyprlock
      monitor
      nixgl
      pam-shim
      parinfer
      powermenu
      wallpaper
      waybar
    ];
  };
}
