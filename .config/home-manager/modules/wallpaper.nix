{
  flake.modules.homeManager.wallpaper = {
    lib,
    config,
    pkgs,
    ...
  }: let
    cfg = config.wallpaper;
  in
    with lib; {
      options.wallpaper = mkOption {
        type = types.attrsOf (types.submodule ({
          name,
          config,
          ...
        }: let
          wallpaperCfg = config;
        in {
          options = {
            angle = mkOption {
              type = types.number;
              default = 0;
            };

            swirl = mkOption {
              type = types.number;
              default = 180;
            };

            gradient = mkOption {
              type = types.submodule {
                options = {
                  beginColor = mkOption {
                    type = types.str;
                  };

                  endColor = mkOption {
                    type = types.str;
                  };
                };
              };
              default = {
                beginColor = "#000000";
                endColor = "#000000";
              };
            };

            logoColor = mkOption {
              type = types.str;
            };

            width = mkOption {
              type = types.number;
              default = 1920;
            };

            height = mkOption {
              type = types.number;
              default = 1080;
            };

            output = mkOption {
              type = types.unspecified;
              readOnly = true;
            };

            outputPath = mkOption {
              type = types.path;
              readOnly = true;
              description = "Path to the generated wallpaper PNG file.";
            };
          };

          config = {
            outputPath = "${wallpaperCfg.output}/share/backgrounds/wallpaper.png";

            output =
              pkgs.runCommandLocal "nix-wallpapers" {
                inherit (wallpaperCfg) width height angle swirl;
                color0 = wallpaperCfg.logoColor;
                color1 = wallpaperCfg.logoColor;
                color2 = wallpaperCfg.logoColor;
                color3 = wallpaperCfg.logoColor;
                color4 = wallpaperCfg.logoColor;
                color5 = wallpaperCfg.logoColor;
                inherit (wallpaperCfg.gradient) beginColor endColor;
                buildInputs = [pkgs.imagemagick];
              } ''
                mkdir -p $out/share/backgrounds
                substituteAll ${../resources/nix-logo.svg} wallpaper.svg

                magick \
                  -size ''${width}x''${height} \
                  -define gradient:angle=''${angle} \
                  gradient:''${beginColor}-''${endColor} \
                  $out/share/backgrounds/gradient.png

                convert \
                  -swirl ''${swirl} \
                  $out/share/backgrounds/gradient.png \
                  -background transparent \
                  -gravity center \
                  wallpaper.svg \
                  -composite \
                  -matte \
                  $out/share/backgrounds/wallpaper.png
              '';
          };
        }));
      };

      config = {
        xdg.dataFile =
          lib.concatMapAttrs (name: cfg: {
            "backgrounds/${name}.png" = {
              source = "${cfg.output}/share/backgrounds/wallpaper.png";
              executable = false;
            };
          })
          cfg;
      };
    };
}
