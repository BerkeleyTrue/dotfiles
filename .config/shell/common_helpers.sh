#!/bin/bash
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

top5commands() {
	history |
		awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' |
		grep -v "./" |
		column -c3 -s " " -t |
		sort -nr |
		nl |
		head -n5
}
