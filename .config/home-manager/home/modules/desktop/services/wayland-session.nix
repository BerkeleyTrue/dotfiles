{...}: {
  systemd.user.targets.wayland-session = {
    Unit = {
      Description = "Wayland Session Services";
      Documentation = "man:systemd.special(7)";
      BindsTo = ["graphical-session.target"];
      Wants = ["graphical-session-pre.target"];
      After = ["graphical-session-pre.target"];
    };
  };
}