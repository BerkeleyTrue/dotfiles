#!/bin/sh
source_shell() {
  source $XDG_CONFIG_HOME/shell/index.sh
}

kll() {
  local pid
  if [[ $# -eq 0 ]]; then
    pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}') || return
  else
    pid=$1
  fi

  if [[ -n "$pid" ]]; then
    echo $pid | xargs kill -9
    return
  fi
}
