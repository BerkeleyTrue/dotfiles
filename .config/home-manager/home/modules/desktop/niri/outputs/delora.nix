{
  hardware,
  node,
  plain,
  leaf,
  flag,
  theme,
  ...
}: let
  inherit (hardware.monitors) g5 dell;
in [
  (node "output" g5.label [
    (leaf "scale" 2.0)
    (leaf "transform" "normal")
    (leaf "mode" "${toString g5.width}x${toString g5.height}@${toString g5.rate}.000")
    (leaf "position" g5.position)
    (leaf "variable-refresh-rate" {on-demand = true;})
    (flag "focus-at-startup")
    (leaf "background-color" theme.colors.lavender)
  ])
  (node "output" dell.label [
    (leaf "scale" 2.0)
    (leaf "transform" "normal")
    (leaf "mode" "${toString dell.width}x${toString dell.height}@${toString dell.rate}.000")
    (leaf "position" dell.position)
    (leaf "background-color" theme.colors.lavender)
  ])
]
