{pkgs, ...}: {
  systemd.user.services.notification-daemon = {
    Unit = {
      Description = "Mako notification daemon";
      PartOf = ["notification.target"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.mako}/bin/mako";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
