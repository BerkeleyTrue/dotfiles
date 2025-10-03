{
  pkgs,
  ...
}: {
  # notification daemon
  systemd.user.services.mako = {
    Unit = {
      Description = "Lightweight Wayland notification daemon";
      After = ["wayland-session.target"];
      PartOf = ["notification.target"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecCondition = ''/bin/sh -c '[ -n "$WAYLAND_DISPLAY" ]' '';
      ExecStart = "${pkgs.mako}/bin/mako";
      ExecReload = "${pkgs.mako}/bin/makoctl reload";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
