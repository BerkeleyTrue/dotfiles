{
  pkgs,
  ...
}: {
  systemd.user.targets.wayland-session = {
    Unit = {
      Description = "Wayland Session";
      Documentation = ["man:systemd.special(7)"];
      BindsTo = ["graphical-session.target"];
      Wants = ["wayland-environment.service" "wayland-foundation.target"];
      After = ["graphical-session-pre.target"];
    };
  };

  systemd.user.services.wayland-environment = {
    Unit = {
      Description = "Wayland Environment Setup";
      Documentation = ["man:systemd.environment(7)"];
      DefaultDependencies = false;
      After = ["graphical-session-pre.target"];
      Before = ["wayland-foundation.target"];
      PartOf = ["wayland-session.target"];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "wayland-environment-setup" ''
        # import wayland environment variables into systemd user environment
        if [ -n "$WAYLAND_DISPLAY" ]; then
          systemctl --user import-environment WAYLAND_DISPLAY
        fi

        # set up wayland-related environment variables
        systemctl --user set-environment XDG_SESSION_TYPE=wayland
        systemctl --user set-environment XDG_CURRENT_DESKTOP=niri
        systemctl --user set-environment XDG_SESSION_DESKTOP=niri

        # notify that wayland environment is ready
        systemd-notify --ready
      '';
    };

    Install = {
      WantedBy = ["wayland-session.target"];
    };
  };

  systemd.user.services.wayland-session = {
    Unit = {
      Description = "Wayland Session Lifecycle Manager";
      Documentation = ["man:niri(1)"];
      After = ["wayland-environment.service"];
      Wants = ["wayland-foundation.target"];
      Conflicts = ["x11-session.target"];
    };

    Service = {
      Type = "notify";
      RemainAfterExit = true;
      TimeoutStartSec = "30s";
      TimeoutStopSec = "10s";

      ExecStart = pkgs.writeShellScript "wayland-session-start" ''
        # Wait for Wayland environment to be ready
        while [ -z "$WAYLAND_DISPLAY" ]; do
          sleep 0.1
        done

        # Signal that Wayland session is ready
        systemd-notify --ready

        # Keep service running while Wayland compositor is active
        while pgrep -x niri >/dev/null; do
          sleep 1
        done
      '';

      ExecStop = pkgs.writeShellScript "wayland-session-stop" ''
        # Graceful shutdown sequence
        systemctl --user stop wayland-foundation.target || true

        # Wait for services to stop cleanly
        timeout 5 bash -c 'while systemctl --user --no-legend --state=deactivating list-units | grep -q .; do sleep 0.1; done' || true
      '';
    };

    Install = {
      WantedBy = ["wayland-session.target"];
    };
  };
}
