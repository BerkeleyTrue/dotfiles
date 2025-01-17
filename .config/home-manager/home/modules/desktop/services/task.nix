{pkgs, ...}: let
  sync = pkgs.writeShellScriptBin "sync-tasks" ''
    TMUX_SESSION=__task-sync__
    DELAY=10
    TASK_SHARE=~/.local/share/task
    SYNC_LOG=$TASK_SHARE/systemd-update.log

    if tmux has-session -t $TMUX_SESSION 2>/dev/null; then
      echo "Stopping previous sync"
    fi

    # first kill previous tmux sync session
    tmux kill-session -t $TMUX_SESSION 2>/dev/null

    # create tmux session, and start sync
    tmux new-session -d -s $TMUX_SESSION "sleep $DELAY && date \"+%F %T\" >> $SYNC_LOG && task sync >> $SYNC_LOG 2>&1 || echo 'Sync failed'" >> $SYNC_LOG
    cat $SYNC_LOG
    # clear sync log
    echo "" > $SYNC_LOG
    exit
  '';
in {
  systemd.user.services.task-sync = {
    Unit = {
      Description = "Run Taskwarrior Sync";
    };

    Service = {
      Type = "simple";
      ExecStart = "${sync}/bin/sync-tasks";
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
