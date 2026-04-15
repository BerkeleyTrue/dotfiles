{
  flake.module.homeManager.xdg = {
    systemd.user.services.xdg-desktop-portal-kde = {
      Unit = {
        Description = "XDG Desktop Portal for KDE";
        Documentation = ["man:xdg-desktop-portal(1)"];
        After = ["wayland-environment.service"];
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

    # needed to load env vars for systemd user services
    xdg.configFile."systemd/user.conf".text = ''
      [Manager]
      ManagerEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
      DefaultEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
    '';
  };
}
