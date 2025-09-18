{...}: {
  systemd.user.targets.x11-session = {
    Unit = {
      Description = "X11 Session Services";
      Documentation = "man:systemd.special(7)";
      BindsTo = ["graphical-session.target"];
      Wants = ["graphical-session-pre.target"];
      After = ["graphical-session-pre.target"];
    };
  };
}