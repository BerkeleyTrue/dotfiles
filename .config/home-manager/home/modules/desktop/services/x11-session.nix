{
  pkgs,
  lib,
  ...
}: {
  systemd.user.targets.x11-session = {
    Unit = {
      Description = "X11 Session";
      Documentation = ["man:systemd.special(7)"];
      BindsTo = ["graphical-session.target"];
      Wants = ["x11-foundation.target"];
      After = ["graphical-session-pre.target"];
    };
  };

  systemd.user.services.x11-session = {
    Unit = {
      Description = "X11 Session Lifecycle Manager";
      Documentation = ["man:startx(1)"];
      Wants = ["x11-foundation.target" "xmonad-session.target"];
      Conflicts = ["wayland-session.target"];
    };

    Service = {
      Type = "notify";
      # needed for systemd-notify to work in scripts
      NotifyAccess = "all";
      RemainAfterExit = true;
      TimeoutStartSec = "30s";
      TimeoutStopSec = "10s";

      ExecStart = let
        script = pkgs.writeShellScriptBin "x11-session-start" ''
          # Wait for X11 environment to be ready
          systemd-notify --status="waiting for X11..."
          while [ -z "$DISPLAY" ] || [ -z "$XAUTHORITY" ]; do
            sleep 0.1
          done

          # Signal that X11 session is ready
          systemd-notify --ready --status="monitoring..."

          # Keep service running while X11 session is active
          while pgrep -x Xorg >/dev/null; do
            sleep 1
          done
        '';
      in "${lib.getExe script}";
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };
}
