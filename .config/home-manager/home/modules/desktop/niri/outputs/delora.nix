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
    (leaf "scale" g5.scale)
    (leaf "transform" "normal")
    (leaf "mode" "${toString g5.width}x${toString g5.height}@${toString g5.rate}.000")
    (leaf "position" g5.position)
    (leaf "variable-refresh-rate" {on-demand = true;})
    (flag "focus-at-startup")
    (leaf "background-color" theme.colors.lavender)

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
]
