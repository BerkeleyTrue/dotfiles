{lib, pkgs, ...}: {

  systemd.user.targets.niri = {
    Unit = {
      Description = "Niri Wayland Compositor";
      Requires = ["wayland-foundation.target"];
      After = ["wayland-foundation.target"];
      BindsTo = ["wayland-session.target"];
      Wants = ["desktop-services.target"];
    };
  };

  systemd.user.targets.niri-session = {
    Unit = {
      Description = "Niri Session";
      Requires = ["niri.target"];
      After = ["niri.target"];
      Wants = ["desktop-services.target"];
      PartOf = ["wayland-session.target"];
    };
  };

  systemd.user.services.niri = {
    Unit = {
      Description = "A scrollable-tiling Wayland compositor";
      BindsTo = ["graphical-session.target"];
      Before = ["graphical-session.target"];
      Wants = ["graphical-session-pre.target"];
      After = ["graphical-session-pre.target"];
    };

    Service = {
      Type = "notify";
      Slice = "session.slice";
      ExecStart = "${lib.getExe pkgs.niri} --session";
    };

    Install.WantedBy = ["niri.target"];
  };
}
