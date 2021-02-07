#!/bin/bash

TIMEWARRIORDB=/home/berkeleytrue/.config/timewarrior /usr/bin/timew summary | awk '{print $1}' | tail -n 2 | head -n 1 |  awk -F: '{print $1 " hr " $2 " mins "}'
