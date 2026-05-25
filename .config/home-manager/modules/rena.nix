{self, ...}: let
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
      framework-tool # Swiss army knife for Framework laptops
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
        inherit (monitors.framework) height width;
      };
    };

    niri.output = let
      inherit (self.kdl) node plain leaf;
      inherit (self) colors;
      inherit (monitors) framework;
    in [
      (node "output" framework.label [
        (leaf "scale" framework.scale)
        (leaf "transform" "normal")
        (leaf "mode" "${toString framework.width}x${toString framework.height}@${toString framework.rate}.000")
        (leaf "background-color" colors.lavender)

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

  configurations.home.rena = {
    inherit username;
    system = "x86_64-linux";
    modules = with homeManager; [
      base
      kanata
      rena
    ];
  };
}
