#!/bin/bash

jcl() {
  journalctl -f -n 100 -u $1
}

jcul() {
  journalctl-f -n 100 --user -u $1
}
