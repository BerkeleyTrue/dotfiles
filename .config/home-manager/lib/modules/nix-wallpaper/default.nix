{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.home.nix-wallpapers;
in
  with lib; {
    options.home.nix-wallpapers = mkOption {
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

          colors = mkOption {
            type = types.submodule {
              options = {
                color0 = mkOption {
                  type = types.str;
                };
                color1 = mkOption {
                  type = types.str;
                };
                color2 = mkOption {
                  type = types.str;
                };
                color3 = mkOption {
                  type = types.str;
                };
                color4 = mkOption {
                  type = types.str;
                };
                color5 = mkOption {
                  type = types.str;
                };
              };
            };
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
        };

        config = {
          output =
            pkgs.runCommandLocal "nix-wallpapers" {
              inherit (wallpaperCfg) width height angle swirl;
              inherit (wallpaperCfg.colors) color0 color1 color2 color3 color4 color5;
              inherit (wallpaperCfg.gradient) beginColor endColor;
              buildInputs = [pkgs.imagemagick];
            } ''
              mkdir -p $out/share/backgrounds
              substituteAll ${./nix-logo.svg} wallpaper.svg

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
  }
