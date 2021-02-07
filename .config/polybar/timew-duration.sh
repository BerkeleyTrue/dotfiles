#!/bin/bash

SUMMARY=$(TIMEWARRIORDB=~/.config/timewarrior /usr/bin/timew summary)

[[ $SUMMARY = No* ]] && echo "0 mins" ||\
  echo "$SUMMARY" \
  | awk '{print $1}' \
  | tail -n 1 \
  | awk -F: '{print ($1=="0")?$2" mins": $1" hrs " $2 " mins" }'
