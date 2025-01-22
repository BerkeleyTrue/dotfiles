{
  pkgs,
  lib,
  ...
}: {
  systemd.user.services.tailscale-systray = {
    Unit = {
      Description = "Tailscale systray";
      Requires = "tray.target";
      After = [
        "graphical-session-pre.target"
        "tray.target"
      ];
      PartOf = ["graphical-session.target"];
    };

    Service = {
      ExecStart = lib.getExe pkgs.tailscale-systray;
    };

    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };
}
