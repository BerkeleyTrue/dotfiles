{
  pkgs,
  config,
  ...
}: let
  wrapper = pkgs.writeShellScript "notification-wrapper" ''
    if [ -n "$WAYLAND_DISPLAY" ]; then
      exec ${pkgs.mako}/bin/mako
    elif [ -n "$DISPLAY" ]; then
      exec ${pkgs.dunst}/bin/dunst -config ${config.xdg.configHome}/dunst/dunstrc
    else
      echo "No display server detected" >&2
      exit 1
    fi
  '';
in {
  systemd.user.services.notification-daemon = {
    Unit = {
      Description = "Notification daemon (dunst/mako wrapper)";
      PartOf = ["notification.target"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${wrapper}";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
