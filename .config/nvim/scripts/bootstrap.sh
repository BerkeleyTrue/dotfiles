#!/usr/bin/env bash

PACKER_DIR=$HOME/.config/nvim/pack/packer/

mkdir -p $PACKER_DIR/start/aniseed
mkdir -p $PACKER_DIR/opt/packer.nvim

if [ ! -d "$PACKER_DIR/opt/packer.nvim" ]; then
  git clone https://github.com/wbthomason/packer.nvim $PACKER_DIR/aniseed
  cd $PACKER_DIR/ && git fetch && git pull
fi

if [ ! -d "$PACKER_DIR/start/aniseed" ]; then
  git clone https://github.com/Olical/aniseed.git $PACKER_DIR/start/aniseed
  cd $PACKER_DIR/start/aniseed && git fetch && git pull
fi
