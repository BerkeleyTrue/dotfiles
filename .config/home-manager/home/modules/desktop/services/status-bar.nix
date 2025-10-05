{
  pkgs,
  lib,
  ...
}: let
  debug = false;
  inspect = false;
in {
  systemd.user.services.taffybar = {
    Unit = {
      Description = "Taffybar Status Bar";
      PartOf = ["tray.target"];
      # we want taffybar to stop before x11-session so it cleanly shuts down.
      # We start x11-session with xmonad.target in xinit so X server should alread be running.
      # xmonad.target -> x11-session.target, which starts grapical-session.target, which starts tray.target
      Before = ["x11-session.target"]; # Start before X11 session stops
      BindsTo = ["x11-session.target"]; # Stop when X11 session stops
      ConditionEnvironment = "DISPLAY";
    };

    Service = {
      Type = "dbus";
      BusName = "org.taffybar.Bar";
      ExecStart = "%h/.local/bin/taffybar";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };

  systemd.user.services.waybar = {
    Unit = {
      Description = "Waybar Status Bar";
      PartOf = ["tray.target"];
      # stame as above but for wayland
      # niri.target -> niri-session.target, which starts grapical-session.target, which starts tray.target
      Before = ["niri-session.target"]; # Start before Niri session stops
      BindsTo = ["niri-session.target"]; # Stop when Niri session stops
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };

    Service = {
      Environment = lib.option inspect "GTK_DEBUG=interactive";
      ExecStart = "${pkgs.waybar}/bin/waybar${lib.optionalString debug "-l debug"}";
      ExecReload = "${pkgs.coreUtils}/bin/kill -SIGUSR2 $MAINPID";
      KillMode = "mixed";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
