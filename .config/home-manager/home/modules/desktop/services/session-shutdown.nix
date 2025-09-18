{...}: {
  systemd.user.targets.session-shutdown = {
    Unit = {
      Description = "Shutdown running graphical session";
      DefaultDependencies = false;
      StopWhenUnneeded = true;
      Conflicts = ["graphical-session.target" "graphical-session-pre.target"];
      After = ["graphical-session.target" "graphical-session-pre.target"];
    };
  };
}
