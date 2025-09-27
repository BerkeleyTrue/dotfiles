{
  ...
}: {
  systemd.user.targets.wayland-foundation = {
    Unit = {
      Description = "Wayland Foundation Services";
      Documentation = ["file:README-systemd-hierarchy.md"];
      After = ["wayland-environment.service"];
      PartOf = ["wayland-session.target"];
    };
  };
}