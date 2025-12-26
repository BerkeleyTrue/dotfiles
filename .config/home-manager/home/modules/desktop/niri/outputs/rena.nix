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
    (leaf "scale" 1.5)
    (leaf "transform" "normal")
    (leaf "mode" "${toString framework.width}x${toString framework.height}@${toString framework.rate}.000")
    (leaf "background-color" theme.colors.lavender)
  ])
]
