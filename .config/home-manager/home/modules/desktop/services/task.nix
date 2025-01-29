{pkgs, ...}: {
  systemd.user.services.task-sync = {
    Unit = {
      Description = "Run Taskwarrior Sync";
    };

    Service = {
      Type = "simple";
      ExecStart = "%h/.local/bin/task-sync";
    };
  };

  systemd.user.timers.task-sync = {
    Unit = {
      Description = "Run Taskwarrior sync periodically";
    };

    Timer = {
      OnBootSec = 80;
      OnCalendar = "*:0/10"; # run every 10 minutes
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };

  systemd.user.services.task-alert-overdue = {
    Unit = {
      Description = "Run Taskwarrior Alert";
    };

    Service = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "taskwarrior-alert" ''
        set -euo pipefail
        count=$(${pkgs.taskwarrior3}/bin/task +OVERDUE count)
        # if there are no overdue tasks, do nothing
        if [ $count -eq 0 ]; then
          echo "No overdue tasks, exiting..."
          exit 0
        fi
        ${pkgs.dunst}/bin/dunstify -a 'Taskwarrior' "You have $count overdue tasks"
      '';
    };
  };

  systemd.user.timers.task-alert-overdue = {
    Unit = {
      Description = "Taskwarrior Alert Sync Timer";
    };

    Timer = {
      OnBootSec = 80;
      OnCalendar = "10..20:00:00"; # run every other hour from 10 to 20
    };

    Install = {
      WantedBy = ["timers.target"];
    };
  };
}
