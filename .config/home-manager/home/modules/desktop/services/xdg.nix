{...}: {
  systemd.user.services.xdg-desktop-portal-kde = {
    Unit = {
      Description = "XDG Desktop Portal for KDE";
      Documentation = ["man:xdg-desktop-portal(1)"];
      After = ["wayland-foundation.target"];
      Before = ["niri.target"];
      PartOf = ["desktop-services.target"];
    };

    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.impl.portal.desktop.kde";
      ExecStart = "/usr/lib/xdg-desktop-portal-kde";
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["desktop-services.target"];
    };
  };
}
