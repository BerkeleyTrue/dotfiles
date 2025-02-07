#!/bin/env bash

# taskwarrior
tadd() {
	task add "$@"
	task
}

tcon() {
	if [[ $# -eq 0 ]]; then
		task context show
		return 0
	fi
	eval "task context $*"
	task
}
# tmod 14 proj:foo
tmod() {
	if [[ $# -eq 0 ]]; then
		echo "no arguments supplied"
		echo "usage: tmod <task-id> <modification> [, ...<modification>]"
		return 1
	fi
	local num=$1
	shift
	eval "task $num mod $*"
	task
}

tdone() {
	if [[ $# -eq 0 ]]; then
		echo "no arguments supplied"
		echo "usage: tdone <task-id>"
		return 1
	fi
	task "$@" "done"
	task
}

tedit() {
	if [[ $# -eq 0 ]]; then
		echo "no arguments supplied"
		echo "usage: tedit <task-id>"
		return 1
	fi
	eval "task $1 edit"
	task
}
