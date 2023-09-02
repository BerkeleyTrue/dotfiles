{pkgs, ...}: {
  systemd.user.services.pasystray = {
    Unit = {
      Description = "Pulse Audio system tray";
      Requires = "tray.target";
      After = [
        "graphical-session-pre.target"
        "tray.target"
      ];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      ExecStart = "${pkgs.pasystray}/bin/pasystray";
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
