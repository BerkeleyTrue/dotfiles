#!/bin/sh
# add alias for MR on command
mrenv() {
  export NODE_PATH=$HOME/dvlpmnt/madison-reed/mr/mr_modules
  export NODE_ENV=local
}

mrinit() {
  mrenv
  /usr/bin/docker start mr-mongo mr-mysql-sanitized redis-server urm_db urm_server > /dev/null
}

mrsd() {
  /usr/bin/docker stop mr-mongo mr-mysql-sanitized redis-server urm_db urm_server > /dev/null
}

mrth() {
  mrinit
  npm run dev-tophat
}

mrcc() {
  mrinit
  npm run dev-cc
}

mrws() {
  mrinit
  npm run dev-website
}

mrpcm() {
  mrinit
  npm run dev-pcm
}

mrcctests() {
  mrinit
  cd ~/dvlpmnt/node/mr/actual/raven
  npm run test
}

mrccstories() {
  mrinit
  cd ~/dvlpmnt/node/mr/actual/raven
  npm run stories
}
