#!/bin/bash

timew_cmd() {
  TIMEWARRIORDB=~/.config/timewarrior /usr/bin/timew $@
}
SUMMARY=$(timew_cmd summary)
IS_ACTIVE=$(timew_cmd get dom.active)
TAG=$(timew_cmd get dom.active.tag.1)

OUTPUT=$(
[[ $SUMMARY = No* ]] && echo "0 mins" ||\
  echo "$SUMMARY" \
  | awk '{print $1}' \
  | tail -n 1 \
  | awk -F: '{print ($1=="0")?$2" mins": $1" hrs " $2 " mins" }'
)

if (( $IS_ACTIVE == "1" )); then
  OUTPUT+=": '"${TAG}"'"
fi

echo $OUTPUT
