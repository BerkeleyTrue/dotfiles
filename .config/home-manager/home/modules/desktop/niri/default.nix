{
  pkgs,
  lib,
  kdl,
  hardware,
  profile,
  ...
}: let
  config = import ./config.nix {
    inherit kdl lib hardware profile;
  };
  validate-config = config:
    pkgs.runCommand "config.kdl"
    {
      inherit config;
      passAsFile = ["config"];
      buildInputs = [pkgs.niri];
    }
    ''
      niri validate -c $configPath
      cp $configPath $out
    '';
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

  xdg.configFile."niri/config.kdl".source = validate-config (kdl.serialize.nodes config);
}
