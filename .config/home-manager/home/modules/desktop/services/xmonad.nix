{...}: {
  systemd.user.targets.xmonad = {
    Unit = {
      Description = "XMonad running";
      Requires = ["x11-session.target"];
      After = ["x11-session.target"];
      BindsTo = ["graphical-session.target" "tray.target"];
    };
  };
}