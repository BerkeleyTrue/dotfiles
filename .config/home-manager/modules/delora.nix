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

    niri.output = let
      inherit (self.kdl) node plain leaf flag;
      inherit (monitors) g5 dell;
    in [
      (node "output" g5.label [
        (leaf "scale" g5.scale)
        (leaf "transform" "normal")
        (leaf "mode" "${toString g5.width}x${toString g5.height}@${toString g5.rate}.000")
        (leaf "position" g5.position)
        (leaf "variable-refresh-rate" {on-demand = true;})
        (flag "focus-at-startup")
        (leaf "background-color" theme.colors.lavender)

        (plain "layout" [
          (plain "preset-column-widths" [
            (leaf "proportion" 0.3333) # 1/3 too small on small monitors
            (leaf "proportion" 0.5) # 1/2
            (leaf "proportion" 0.6667) # 2/3
            (leaf "proportion" 0.75) # 3/4
          ])
        ])
      ])
      (node "output" dell.label [
        (leaf "scale" dell.scale)
        (leaf "transform" "normal")
        (leaf "mode" "${toString dell.width}x${toString dell.height}@${toString dell.rate}.000")
        (leaf "position" dell.position)
        (leaf "background-color" theme.colors.lavender)

        (plain "layout" [
          (plain "preset-column-widths" [
            (leaf "proportion" 0.5) # 1/2
            (leaf "proportion" 0.6667) # 2/3
            (leaf "proportion" 0.75) # 3/4
          ])
        ])
      ])
    ];
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
