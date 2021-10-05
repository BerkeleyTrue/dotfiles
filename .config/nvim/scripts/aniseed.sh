#!/usr/bin/env bash

ANISEED_DIR=pack/packer/start
mkdir -p $ANISEED_DIR

if [ ! -d "$ANISEED_DIR/aniseed" ]; then
  git clone https://github.com/Olical/aniseed.git $ANISEED_DIR/aniseed
fi

cd $ANISEED_DIR/aniseed && git fetch && git checkout tags/v3.16.0
