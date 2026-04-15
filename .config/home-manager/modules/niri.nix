{self, ...}: let
  inherit (self) kdl;
in {
  flake.modules.homeManager.niri = {
    pkgs,
    lib,
    config,
    ...
  }: let
    nixGlWrap = config.lib.nixGL.wrap;
    niri = nixGlWrap pkgs.niri;
    niri-config = config.niri.niri-config;
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

    systemd.user.targets.niri = {
      Unit = {
        Description = "Niri Wayland Compositor";
        BindsTo = ["graphical-session.target"];
        After = ["wayland-environment.service"];
        Wants = ["desktop-services.target"];
      };
    };

    systemd.user.services.niri = {
      Unit = {
        Description = "A scrollable-tiling Wayland compositor";
        BindsTo = ["graphical-session.target"];
        Before = ["graphical-session.target"];
        Wants = ["graphical-session-pre.target"];
        After = ["graphical-session-pre.target"];

        # Prevent home-manager from restarting the compositor during switch,
        # which would terminate the current session
        X-RestartIfChanged = false;
        X-StopIfChanged = false;
      };

      Service = {
        Type = "notify";
        Slice = "session.slice";
        ExecStart = "${lib.getExe niri} --session";
      };

      Install.WantedBy = ["niri.target"];
    };
  };
}
