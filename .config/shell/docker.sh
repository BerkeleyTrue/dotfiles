#!/bin/bash

doc-pull-n-tty() {
  set -u
  local arg=$1

  docker pull $arg && docker run -i --tty --entrypoint /bin/sh $arg
}
