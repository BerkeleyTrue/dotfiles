{}: let
  colors = import ./colors.nix;
in {
  # #RRGGBB
  inherit colors;
  colorsRgb = builtins.mapAttrs (_: v: let c = builtins.substring 1 6 v; in "rgb(${c})") colors;
}
