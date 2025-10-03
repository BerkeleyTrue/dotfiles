{kdl, ...}: let
  config = import ./config.nix {inherit kdl;};
in {
  xdg.configFile."niri/config.kdl".text = kdl.serialize.nodes config;
}
