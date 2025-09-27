{
  pkgs,
  ...
}: {
  systemd.user.targets.xmonad = {
    Unit = {
      Description = "XMonad Window Manager";
      Documentation = ["man:xmonad(1)"];
      Requires = ["x11-foundation.target"];
      After = ["x11-foundation.target"];
      BindsTo = ["x11-session.target"];
      Wants = ["desktop-services.target"];
    };
  };

  systemd.user.targets.xmonad-session = {
    Unit = {
      Description = "XMonad Session";
      Documentation = ["man:systemd.special(7)"];
      Requires = ["xmonad.target"];
      After = ["xmonad.target"];
      Wants = ["desktop-services.target"];
      PartOf = ["x11-session.target"];
    };
  };

  systemd.user.services.xmonad = {
    Unit = {
      Description = "XMonad Window Manager";
      Documentation = ["man:xmonad(1)"];
      After = ["x11-foundation.target"];
      PartOf = ["xmonad.target"];
      OnFailure = ["xmonad-failure-handler.service"];
    };

    Service = {
      Type = "simple";
      ExecStart = "%h/.local/bin/xmonad-x86_64-linux";
      Restart = "on-failure";
      RestartSec = 2;
      TimeoutStartSec = "10s";
      TimeoutStopSec = "5s";
      
      # Environment for XMonad
      Environment = [
        "DESKTOP_SESSION=xmonad"
        "XDG_CURRENT_DESKTOP=XMonad" 
        "XDG_SESSION_DESKTOP=XMonad"
        "XDG_SESSION_TYPE=x11"
      ];
    };

    Install = {
      WantedBy = ["xmonad.target"];
    };
  };

  # xmonad failure handler for logging and recovery
  systemd.user.services.xmonad-failure-handler = {
    Unit = {
      Description = "XMonad Failure Handler";
      Documentation = ["man:systemd.service(5)"];
    };

    Service = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "xmonad-failure-handler" ''
        # Log the failure
        echo "$(date): XMonad failed, attempting recovery" >> ~/.local/share/x11/xmonad-failures.log
        
        # Check if X11 is still running
        if ! pgrep -x Xorg >/dev/null; then
          echo "$(date): X11 server is dead, stopping session" >> ~/.local/share/x11/xmonad-failures.log
          systemctl --user stop x11-session.target
        fi
      '';
    };
  };
}
