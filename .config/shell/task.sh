#!/bin/sh

# taskwarrior
alias tadd='task add'

tcon() {
  if [[ $# -eq 0 ]]; then
    task context show;
    return 0;
  fi
  eval "task context $*"
}
# tmod 14 proj:foo
tmod() {
  if [[ $# -eq 0 ]]; then
    echo "no arguments supplied";
    echo "usage: tmod <task-id> <modification> [, ...<modification>]"
    return 1;
  fi
  local num=$1
  shift
  eval "task $num mod $*"
}

tdone() {
  if [[ $# -eq 0 ]]; then
    echo "no arguments supplied";
    echo "usage: tdone <task-id>"
    return 1;
  fi
  eval "task $1 done"
}

tedit() {
  if [[ $# -eq 0 ]]; then
    echo "no arguments supplied";
    echo "usage: tedit <task-id>"
    return 1;
  fi
  eval "task $1 edit"
}
