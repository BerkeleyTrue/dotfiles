{pkgs, ...}: {
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
}
