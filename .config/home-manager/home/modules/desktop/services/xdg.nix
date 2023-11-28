{...}: {
  systemd.user.services.xdg-desktop-portal-kde = {
    Unit = {
      Description = "XDG Portal service for KDE";
    };

    Service = {
      Type = "dbus";
      BusName= "org.freedesktop.impl.portal.desktop.kde";
      ExecStart = "/usr/lib/xdg-desktop-portal-kde";
    };
  };
}
