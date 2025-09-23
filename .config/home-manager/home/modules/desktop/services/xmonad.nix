{...}: {
  systemd.user.targets.xmonad = {
    Unit = {
      Description = "XMonad running";
      Requires = "graphical-session-pre.target";
      BindsTo = "graphical-session.target tray.target";
    };
  };
}