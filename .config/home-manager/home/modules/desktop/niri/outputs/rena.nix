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
    (leaf "primary" true)
    (leaf "scale" 2.0)
    (leaf "transform" "normal")
    (leaf "mode" "${framework.width}x${framework.height}@${framework.rate}.000")
    (leaf "position" framework.position)
  ])
]
