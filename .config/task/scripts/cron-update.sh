#!/bin/sh
TASK_BACKLOG=~/.config/task/backlog.data
TASK_SHARE=~/.local/share/task
SYNC_LOG=$TASK_SHARE/cron-update.log
LOCK_FILE=$TASK_SHARE/autosync.lock

if [ ! -f $LOCK_FILE ]; then
  touch $LOCK_FILE

  output=$(task sync)
  echo "$(date "+\%F \%T") - $output" >>$SYNC_LOG 2>&1

  rm $LOCK_FILE
fi

exit 0
