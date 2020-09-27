#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

# Launch top and bottom for main monitor

echo "---" | tee -a /tmp/polybar1.log
polybar -r top >>/tmp/polybar1.log 2>&1 &

echo "---" | tee -a /tmp/polybar2.log
polybar -r bottom >>/tmp/polybar2.log 2>&1 &

COUNT=3
if (( $(polybar -m | wc -l) > 1 )); then
  for m in $(polybar -m | grep -v primary | sed 's/:.*$//g'); do

    echo "---" | tee -a /tmp/polybar$COUNT.log
    echo "starting monitor: $m; count: $COUNT" | tee -a /tmp/polybar$COUNT.log
    MONITOR=$m polybar -r multitop >>/tmp/polybar$COUNT.log 2>&1 &

    COUNT=$((COUNT+1))
  done
fi

echo "Bars launched..."
