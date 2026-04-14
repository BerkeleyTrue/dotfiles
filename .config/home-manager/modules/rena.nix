{
  pkgs,
  self,
  ...
}: let
  inherit (self) colors;
  inherit (self.modules) homeManager;
  username = "bt";
in {
  # main workstation
  flake.modules.homeManager.rena = {
    config,
    pkgs,
    lib,
    ...
  }: let
    monitors = config.monitors;
    common-modules = config.waybar.commonModules;
  in {
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

    programs.waybar.settings = {
      renaTop = let
        height = 34;
      in
        {
          inherit height;
          name = "rena-top";
          layer = "bottom";

          modules-left = ["niri/workspaces"];
          modules-center =
            lib.intersperse "custom/separator"
            ["custom/wttr" "clock"];
          modules-right =
            lib.intersperse "custom/separator"
            [
              "custom/powermenu"
              "cpu"
              "custom/cpu-temp"
              "memory"
            ]
            ++ ["tray"];
        }
        // common-modules;
      bottom =
        {
          inherit height;
          name = "rena-bottom";
          layer = "bottom";
          position = "bottom";

          modules-left = ["clock#date"];
          modules-center =
            lib.intersperse "custom/separator"
            ["custom/wakatime" "custom/eth" "custom/btc"];
          modules-right =
            lib.intersperse "custom/separator"
            [
              "custom/swaync"
              "pulseaudio"
              "battery"
              "custom/connectivity"
            ];

          battery = {
            states = {
              good = 100;
              warning = 30;
              danger = 20;
            };
            interval = 1;
            tooltip = true;
            format = "{icon} {capacity}%";
            format-charging = "<b>󰂄 {capacity}%</b>";
            format-icons = [
              "󰁺"
              "󰁻"
              "󰁼"
              "󰁽"
              "󰁾"
              "󰁿"
              "󰂀"
              "󰂁"
              "󰂂"
              "󰁹"
            ];
          };
        }
        // common-modules;
    };
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
      desktop-apps
      fonts
      hyprlock
      monitor
      neovim
      nixgl
      pam-shim
      parinfer
      powermenu
      wallpaper
      waybar
    ];
  };
}
