{...}: {
  systemd.user.targets.x11-session = {
    Unit = {
      Description = "X11 Session";
      Documentation = ["man:systemd.special(7)"];
      BindsTo = ["graphical-session.target"];
      Wants = ["x11-foundation.target"];
      After = ["graphical-session-pre.target"];
    };
  };
}
