{
  lib,
  flake-parts-lib,
  ...
}: let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) mkOption types;
in {
  options = mkPerSystemOption ({
    config,
    pkgs,
    ...
  }: let
    cfg = config.wallpaper;
  in {
    options = {
      wallpaper.angle = {
        type = types.number;
        default = 0;
        description = ''
          the angle of the gradient to north
        '';
      };

      wallpaper.gradient = {
        type = types.string;
        default = "black-white";
        description = ''
          the gradient to use for the wallpaper
        '';
      };

      wallpaper.colors = {
        type = types.submodule {
          options = {
            color1 = {
              type = types.string;
              default = "#000000";
              description = ''
                the first color of the nix logo
              '';
            };
            color2 = {
              type = types.string;
              default = "#000000";
              description = ''
                the second color of the nix logo
              '';
            };
            color3 = {
              type = types.string;
              default = "#000000";
              description = ''
                the third color of the nix logo
              '';
            };
            color4 = {
              type = types.string;
              default = "#000000";
              description = ''
                the fourth color of the nix logo
              '';
            };
            color5 = {
              type = types.string;
              default = "#000000";
              description = ''
                the fifth color of the nix logo
              '';
            };
            color6 = {
              type = types.string;
              default = "#000000";
              description = ''
                the sixth color of the nix logo
              '';
            };
          };
        };
        description = ''
          colors for the nix logo wallpaper
        '';
      };

      wallpaper.build = mkOption {
        type = types.package;
        readOnly = true;
        description = ''
          build a nix logo wallpaper using the current monitors resolution
        '';
      };
    };

    config = let
      svg = pkgs.substituteAll {
        src = ./nix-logo.svg;
        name = "logo";
        inherit (cfg.colors) color1 color2 color3 color4 color5 color6;
      };
    in {
      wallpaper.build = pkgs.writeShellApplication {
        name = "gen-wallpaper";
        runtimeInputs = [pkgs.imagemagic];
        text = ''
          angle=${toString cfg.angle}
          screen_size=$(xrandr | grep '*' | awk '{print $1}')

          echo "generating wallpaper with angle $angle and screen size $screen_size"

          magick \
            -size $screen_size \
            -define gradient:angle=$angle \
            gradient:${cfg.gradient} \
            /tmp/gradient.png


          convert \
            -swirl 180 \
            /tmp/gradient.png \
            -background transparent \
            -gravity center \
            ${svg} \
            -composite \
            -matte \
            ./wallpaper.png
        '';
      };
    };
  });
}
