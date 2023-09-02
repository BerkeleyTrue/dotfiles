{...}: {
  systemd.user.targets.tray = {
    Unit = {
      Description = "System Tray";
      Requires = "graphical-session-pre.target";
    };
  };
}
