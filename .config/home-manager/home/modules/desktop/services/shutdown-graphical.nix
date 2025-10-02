{...}: {
  systemd.user.targets.shutdown-graphical = {
    Unit = {
      Description = "Shutdown graphical session";
      DefaultDependencies = "no";
      StopWhenUnneeded = true;

      After = ["graphical-session.target" "graphical-session-pre.target"];
      Conflicts = ["graphical-session.target" "graphical-session-pre.target"];
    };
  };
}
