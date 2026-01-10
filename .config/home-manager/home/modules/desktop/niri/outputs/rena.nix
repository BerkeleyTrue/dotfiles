{
  hardware,
  node,
  plain,
  leaf,
  flag,
  theme,
  ...
}: let
  inherit (hardware.monitors) framework;
in [
  (node "output" framework.label [
    (leaf "scale" framework.scale)
    (leaf "transform" "normal")
    (leaf "mode" "${toString framework.width}x${toString framework.height}@${toString framework.rate}.000")
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
