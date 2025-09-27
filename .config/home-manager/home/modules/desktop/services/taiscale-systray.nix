{
  pkgs,
  lib,
  ...
}: {
  systemd.user.services.tailscale-systray = {
    Unit = {
      Description = "Tailscale System Tray";
      Documentation = ["man:tailscale(1)"];
      After = ["tray.target"];
      PartOf = ["tray.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = lib.getExe pkgs.tailscale-systray;
      Restart = "on-failure";
      RestartSec = 3;
    };

    Install = {
      WantedBy = ["tray.target"];
    };
  };
}
