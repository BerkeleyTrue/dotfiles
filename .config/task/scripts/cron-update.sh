#!/bin/sh
TASK_BACKLOG=~/.config/task/backlog.data
TASK_SHARE=~/.local/share/task
SYNC_LOG=$TASK_SHARE/cron-update.log
LOCK_FILE=$TASK_SHARE/SYNC.lock

if {
  # set C ensures redirect cannot overwrite a file.
  set -C
  # create lock file if it doesn't exist, pipe errors to /dev/null
  2>/dev/null >$LOCK_FILE
}; then
  # trap on exit to remove lockfile
  trap 'rm -f "$LOCK_FILE"' EXIT

  output=$(task sync)
  echo "$(date "+\%F \%T") - $output" >>$SYNC_LOG 2>&1
fi
exit
