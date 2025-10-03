{
  pkgs,
  kdl,
  ...
}: let
  config = import ./config.nix {inherit kdl;};
in {
  home.packages = with pkgs; [
    niri
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
    configPackages = with pkgs; [
      niri
    ];
  };

  xdg.configFile."niri/config.kdl".text = kdl.serialize.nodes config;
}
