#!/bin/sh

#Makes and CD's to a new directoy.
function mkdirandcd() {
  mkdir -pv $1
  cd $_
}

lpasssearch() {
  lpass show -c --password $(lpass ls | fzf | awk '{print $(NF)}' | sed 's/\]//g')
}

cdfzf() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) &&
    cd "$dir"
}

