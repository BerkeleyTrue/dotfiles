{pkgs, ...}: let
  sync = pkgs.writeScriptBin "sync-tasks" ''
    #!/bin/bash
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
    tmux new-session -d -s $TMUX_SESSION "sleep $DELAY && date \"+\%F \%T\" >> $SYNC_LOG && task sync >> $SYNC_LOG 2>&1 || echo 'Sync failed'" >> $SYNC_LOG
    cat $SYNC_LOG
    exit
  '';
in {
  systemd.user.services.task-sync = {
    Unit = {
      Description = "Run Taskwarrior sync";
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
}
