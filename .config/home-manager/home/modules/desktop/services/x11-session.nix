{pkgs, ...}: {
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
      RemainAfterExit = true;
      TimeoutStartSec = "30s";
      TimeoutStopSec = "10s";

      ExecStart = pkgs.writeShellScript "x11-session-start" ''
        # Wait for X11 environment to be ready
        echo "Waiting for X11 environment... $DISPLAY $XAUTHORITY"
        while [ -z "$DISPLAY" ] || [ -z "$XAUTHORITY" ]; do
          sleep 0.1
        done

        # Signal that X11 session is ready
        systemd-notify --ready

        # Keep service running while X11 session is active
        while pgrep -x Xorg >/dev/null; do
          sleep 1
        done
      '';

      ExecStop = pkgs.writeShellScript "x11-session-stop" ''
        # Graceful shutdown sequence
        systemctl --user stop xmonad-session.target || true
        systemctl --user stop x11-foundation.target || true

        # Wait for services to stop cleanly
        timeout 5 bash -c 'while systemctl --user --no-legend --state=deactivating list-units | grep -q .; do sleep 0.1; done' || true
      '';
    };

    Install = {
      WantedBy = ["x11-session.target"];
    };
  };
}
