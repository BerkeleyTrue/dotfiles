#!/bin/bash

doc-tty() {
  local arg=$1
  if [ -z "$arg" ]; then
    echo "Requires image url but found none" 1>&2
    return 1
  fi

  docker run -i --tty --entrypoint /bin/sh $arg
}

doc-pull-n-tty() {
  local arg=$1
  if [ -z "$arg" ]; then
    echo "Requires image url but found none" 1>&2
    return 1
  fi

  docker pull $arg && doc-tty $arg
}
