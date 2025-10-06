{
  hardware,
  node,
  plain,
  leaf,
  flag,
  ...
}: let
  inherit (hardware.monitors) framework;
in [
  (node "output" framework.label [
    (leaf "scale" 2.0)
    (leaf "transform" "normal")
    (leaf "mode" "${toString framework.width}x${toString framework.height}@${toString framework.rate}.000")
  ])
]
