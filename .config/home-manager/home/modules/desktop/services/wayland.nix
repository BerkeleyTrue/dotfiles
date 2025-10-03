{
  ...
}: {
  systemd.user.targets.wayland-foundation = {
    Unit = {
      Description = "Wayland Foundation Services";
      After = ["wayland-environment.service"];
      PartOf = ["wayland-session.target"];
    };
  };

  systemd.user.targets.wayland-session = {
    Unit = {
      Description = "Wayland Session";
      BindsTo = ["graphical-session.target"];
      Wants = ["wayland-environment.service" "wayland-foundation.target"];
      After = ["graphical-session-pre.target"];
    };
  };
}
