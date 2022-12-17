#!/bin/bash
TASK_BACKLOG=~/.config/task/backlog.data
TASK_SHARE=~/.local/share/task
SYNC_LOG=$TASK_SHARE/cron-update.log
TMUX_SESSION=task-sync
DELAY=10

if ! tmux has-session -t $TMUX_SESSION 2>/dev/null; then
  echo "Stopping previous sync" >> $SYNC_LOG
fi

# first kill previous tmux sync session
tmux kill-session -t $TMUX_SESSION 2>/dev/null

# create tmux session, and start sync
tmux new-session -d -s $TMUX_SESSION "sleep $DELAY && date \"+\%F \%T\" >> $SYNC_LOG && task sync >> $SYNC_LOG 2>&1\
    || echo 'Sync failed' >> $SYNC_LOG"

exit
