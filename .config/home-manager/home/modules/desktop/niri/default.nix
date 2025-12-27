{
  pkgs,
  lib,
  kdl,
  hardware,
  profile,
  theme,
  config,
  ...
}: let
  niri = config.lib.nixgl.wrapPackage pkgs.niri;
  niri-config = import ./config.nix {
    inherit kdl lib hardware profile theme;
  };
  validate-config = config:
    pkgs.runCommand "config.kdl"
    {
      config = config;
      passAsFile = ["config"];
      buildInputs = [pkgs.niri];
    }
    ''
      niri validate -c $configPath
      cp $configPath $out
    '';
in {
  home.packages = [
    niri
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
    configPackages = [
      niri
    ];
  };

  xdg.configFile."niri/config.kdl".source = validate-config (kdl.serialize.nodes niri-config);
}
