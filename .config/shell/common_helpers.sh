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

zsh_edit_history() {
  nvim ~/.local/share/zsh/.zsh_history
}

_syncdir() {
  cd $1
  echo
  echo "===-<Syncing $(pwd)>-==="
  git pull
  git push
  echo "===-<done>-==="
}

notessync() {
  set +m
  declare -A outputs=()
  local msg="===-<output>-===\n"
  local dirs=(
    $HOME/docs/corpus
    $HOME/docs/notes/corpus
    $HOME/docs/notes/captainslog
    $HOME/dvlpmnt/node/mr/notes
  )

  keybase ctl start

  echo "===-<starting>-==="
  for dir in ${dirs[@]}; do
    outputs[$dir]=$(mktemp /tmp/notessync.XXX)
    { _syncdir $dir &>$outputs[$dir] & } 2>/dev/null
  done

  wait

  for dir in ${dirs[@]}; do
    msg+="$(cat $outputs[$dir])\n"
  done

  echo $msg
  set -m
}

top5commands() {
  history |
    awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' |
    grep -v "./" |
    column -c3 -s " " -t |
    sort -nr |
    nl |
    head -n5
}
