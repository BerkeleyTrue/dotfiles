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
    inherit kdl lib hardware profile theme pkgs;
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
    pkgs.xwayland-satellite
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-termfilechooser
      xdg-desktop-portal-gtk
    ];
    config.common = {
      "org.freedesktop.impl.portal.FileChooser" = "termfilechooser";
    };
    configPackages = [
      niri
    ];
  };

  xdg.configFile."niri/config.kdl".source = validate-config (kdl.serialize.nodes niri-config);
  xdg.configFile."xdg-desktop-portal-termfilechooser/config".text = ''
    [filechooser]
    cmd=${pkgs.xdg-desktop-portal-termfilechooser}/share/xdg-desktop-portal-termfilechooser/yazi-wrapper.sh
    env=TERMCMD=kitty --class "terminal-file-picker"
    default_dir=$HOME
    open_mode=suggested
    save_mode=last
  '';
}
