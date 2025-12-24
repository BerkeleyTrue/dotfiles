{
  lib,
  pkgs,
  config,
  ...
}: let
  wrappedNiri = config.lib.nixgl.wrapPackage pkgs.niri;
in {
  systemd.user.targets.niri = {
    Unit = {
      Description = "Niri Wayland Compositor";
      BindsTo = ["graphical-session.target"];
      After = ["wayland-environment.service"];
      Wants = ["desktop-services.target"];
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
      ExecStart = "${lib.getExe wrappedNiri} --session";
    };

    Install.WantedBy = ["niri.target"];
  };
}
