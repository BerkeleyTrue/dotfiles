#! /bin/bash

#git aliases
# +++++++++++++++++++++++++++++++++++++++++++++{{{
alias gstat='git status'
alias tstat='tig status'
alias gfetch='git fetch && git status'
alias gpush='git push'
alias gpushntrack='git push -u'
alias gdiff='tdiff'
alias gcom='git commit'
alias gamend='git commit --amend'
alias glog='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
alias glognum='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
alias gtagls='git describe --tags --abbrev=0'
alias gtag='git tag -s'
alias grst='git reset'
alias grst1='git reset HEAD^'
alias grst2='git reset HEAD^^'
alias grsth='git reset --hard'
alias grsth1='git reset --hard HEAD^'
alias grsth2='git reset --hard HEAD^^'
alias grsts='git reset --soft'
alias grsts1='git reset --soft HEAD^'
alias grsts2='git reset --soft HEAD^^'
alias gstash='git stash'
#}}}

# github cli
# +++++++++++++++++++++++++++++++++++++++++++++{{{
alias ghcopr='gh pr checkout'
#}}}

# yadm aliases
# +++++++++++++++++++++++++++++++++++++++++++++{{{
alias ycom='yadm commit'
alias yamend='yadm commit --amend'
alias yco='yadm checkout'
alias ydiff='yadm diff'
alias ydfcached='yadm diff --cached'
alias yfetch='yadm fetch'
alias ylog='yadm log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
alias ypull='yadm pull --rebase'
alias ypush='yadm push'

alias yrst='yadm reset'
alias yrst1='yadm reset HEAD^'
alias yrst2='yadm reset HEAD^^'
alias yrsth='yadm reset --hard'
alias yrstsubmodules='yadm submodule deinit -f .; yadm submodule update --init'
alias yrsth1='yadm reset --hard HEAD^'
alias yrsth2='yadm reset --hard HEAD^^'
alias yrsts='yadm reset --soft'
alias yrsts1='yadm reset --soft HEAD^'
alias yrsts2='yadm reset --soft HEAD^^'

# add unignored files to intent to add
yunignored() {
	unignore="$XDG_CONFIG_HOME/yadm/unignored"
	test -r "$unignore" && cat "$unignore" | envsubst | yadm add --intent-to-add --pathspec-from-file=-
}
# yadm status w/ unignored files
ystat() {
	yunignored
	yadm status
}
#
# yadm add using fzf including unignored files
yadd() {
	# if args are provided, assume those are args to yadm add
	# and pass them through, exiting early
	if [ $# -gt 0 ]; then
		yadm add "$@"
		return
	fi

	# if no args, go into fzf of changed files
	# can't do -o for yadm since it will list all the files

	# add unignored files to intent to add
	yunignored

	# user selects files to add
	toAdd=$(yadm ls-files -m --exclude-standard | fzf -m --print0 --preview "yadm --no-pager diff {} | bat --color always")

	# if toAdd is empty, exit
	if [ -z "$toAdd" ]; then
		return
	fi

	$(echo -n $toAdd | xargs -0 -o -t yadm add &>/dev/null)

	yadm status
}
#}}}

gadd() {
	# if no args, gadd goes into fzf of changed files
	if [[ $# -eq 0 ]]; then
		git ls-files -m -o --exclude-standard | fzf -m --print0 --preview "bat --color always --diff  {}" | xargs -0 -o -t git add
		return 0
	fi
	# regular git add if arguments are given
	eval "git add $@"
}

ggetcurrentbranch() {
	git rev-parse --abbrev-ref HEAD
}
grename() {
	old_name=$1
	new_name=$2
	git mv --force $old_name tmp
	git mv --force tmp $new_name
}

gcorbranch() {
	# gcor feat/foo
	# = git checkout -b origin feat/foo
	# gcor feat/foo upstream
	git fetch ${2:-origin}
	git checkout -b $1 ${2:-origin}/$1
}
gcorpullrequest() {
	# gcorpullrequest 123  upstream
	# gcorpullrequest 124
	remote=origin
	if [ $# = 2 ]; then
		remote=$2
	fi

	echo "What do you want to call the local branch?"
	read branchname

	echo "fetching PR #$1 from $remote"
	echo "Do you want to continue?"
	select yn in "Yes" "No"; do
		case $yn in
		Yes) break ;;
		No) return 0 ;;
		esac
	done

	git fetch $remote pull/$1/head:$branchname
	git checkout $branchname
}
gresetrpullrequest() {
	# gresetrpullrequest $issue-number $remote-name=origin
	remote=origin
	if [ $# = 2 ]; then
		remote=$2
	fi
	branchname=$(ggetcurrentbranch)
	head=$(git symbolic-ref refs/remotes/$remote/HEAD | sed 's@^refs/remotes/origin/@@')
	echo "HEAD points to $head"
	echo "branch points to $branchname"
	echo "PR number $1"
	echo "continue?"
	select yn in "Yes" "No"; do
		case $yn in
		Yes) break ;;
		No) return 0 ;;
		esac
	done
	echo "switching to $head"
	git checkout $head
	echo "deleting old branch"
	git branch -D $branchname
	echo "fetching PR"
	git fetch $remote pull/$1/head:$branchname
	echo "switching to updated $branchname"
	git checkout $branchname
}

gbranch() {
	# if given command `gbranch`
	# list out branches
	if [ $# = 0 ]; then
		git branch
		return 0
	fi
	echo "creating branch '$1'"
	# given the command `gbranch berks-is-awesome`
	# creates a new branch `berks-is-awesome`
	# checks that branch out
	git checkout -b $1
	echo "creating '${2:-origin}/$1'"
	# this will make the current branch track origin/berks-is-awesome
	git push -u ${2:-origin} $1
}
gremote() {
	# gremote
	if [[ $# -eq 0 ]]; then
		echo "printing remote repositories"
		git remote -v
		return 0
	fi
	while test $# -gt 0; do
		case "$1" in
		-h | --help)
			echo "gremote - a better git remote"
			echo "gremote [options] [commands]"
			echo "options:"
			echo "-h, --help  list options and commands"
			echo "commands:"
			echo "add         gremote add remote-name url - Adds a new remote with url"
			echo "set         gremote set remote-name url - set or overwrite existing remote url"
			echo "remove      gremote remove remote-name  - remote remote repo"
			return 0
			;;
		add)
			if [[ -z "${2///}" || "${#2}" -lt 3 ]]; then
				echo 'remote name must be atleast three characters'
				return 2
			fi
			# check if url is actual repo
			if ! git ls-remote --exit-code $3 &>/dev/null; then
				echo 'remote url does not resolve to proper git repo'
				return 2
			fi
			git remote add $2 $3
			echo "success"
			echo "printing remote repositories"
			git remote -v
			return 0
			;;
		set)
			# gremote set remote-name url
			# check if url is actual repo
			if ! git ls-remote --exit-code $3 &>/dev/null; then
				echo 'remote url does not resolve to proper git repo'
				return 2
			fi
			git remote set-url $2 $3
			echo "printing remote repositories"
			git remote -v
			return 0
			;;
		remove)
			# gremote remove remote-name
			echo "removing $2"
			git remote remove $2
			echo "remove successful"
			echo "printing remote repositories"
			git remote -v
			return 0
			;;
		rename)
			# gremote remove remote-name
			echo "renaming $2"
			git remote rename $2 $3
			git remote -v
			return 0
			;;
		*)
			echo "gremote: $1 is not a proper command"
			return 2
			;;
		esac
	done
}
grebase() {
	# grebase
	if [[ $# -eq 0 ]]; then
		echo "grebase: Incorrect number of args"
		return 1
	fi

	if [[ $1 == 'cont' ]]; then
		echo "continuing..."
		git rebase --continue
		return 0
	fi

	if [[ $1 == 'skip' ]]; then
		echo "skiping..."
		git rebase --skip
		return 0
	fi

	if [[ $1 == 'abort' ]]; then
		echo "aborting..."
		git rebase --abort
		return 0
	fi
	git rebase $1
}
gclone() {
	# Will clone a repo into a new dir as the second arg
	git clone $1 $2
	# and it will cd into the newly created repo dir
	wd=$(pwd)
	echo "changing directory to $wd/${2:-$(basename $1 .git)}"
	cd ${2:-$(basename $1 .git)}
}
gclonewt() {
	# clone a bare repo for working tree workflow
	# glonewt <repo> [dir]
	repo=$1
	shift

	if [ -z "$repo" ]; then
		echo "Usage: git-clone-bare repo [dir]"
		exit 1
	fi

	dir=${1:-$(basename "$repo" .git)}

	echo "bare cloning $repo into ./$dir"

	git clone --bare $repo "./$dir/.bare"
	echo "changing working directory to $(pwd)/$dir"
	cd "./$dir"
	pushd "./.bare" >/dev/null
	echo "adjusting origin fetch url"
	git config remote.origin.fetch "+refs/heads/*:refs/heads/origin/*"
	popd >/dev/null
	echo "settings .git config to bare"
	echo "gitdir: ./.bare" >.git
}
alias gwtls='git worktree list'
alias gwtmv='git worktree move'
gwta() {
	# gwta path [branch=$(basename path)] [remote=origin]
	_path=$1
	_branch=${2:-$(basename $_path)}
	_remote=${3:-origin}
	git worktree add $_path $_branch
	cd $_path
	git fetch
	git branch --set-upstream-to=origin/$_branch $_branch
	git worktree list
}
gwtbranch() {
	# gwtbranch path [branch=$(basename path)] [branch_from=master] [remote=origin]
	_path=$1
	shift
	_branch=$(basename $_path)
	_branch_from=$(ggetcurrentbranch || echo "master")
	_remote=origin

	if [ $# -ge 1 ]; then
		_branch=$1
	fi

	if [ $# -ge 2 ]; then
		_branch_from=$2
	fi

	if [ $# -ge 3 ]; then
		_remote=$3
	fi
	echo "adding worktree $_path from $_branch_from to $_branch from $_remote"

	git worktree add -b $_branch $_path $_branch_from &&
		echo "worktree added" &&
		cd $_path &&
		git push -u origin $_branch &&
		git worktree list
}
gwtrm() {
	# gwtrm path
	_path=$1
	git worktree remove $_path
	git worktree list
}
gpull() {
	# if no arguments, pull from the repository this branch is tracking by rebase
	if [[ $# -eq 0 ]]; then
		git pull --rebase
		return 0
	fi

	# If given gpull <remote>
	# find the current branch and pull that branch
	# from the <remote> remote repository
	currentbranch=$(ggetcurrentbranch)

	# force git pull (overrides local changes)
	if [[ $1 == "-f" ]]; then
		local remote=$(git remote)
		git fetch --all
		git reset --hard $remote/$currentbranch
		return 0
	fi

	echo "current branch is $currentbranch"
	echo "pulling down branch '$currentbranch' from remote repository '$1'"
	git pull $1 $currentbranch
}

gclean() {
	# will remove local merged refs
	# and delete those branches on the remote
	# can be used `glean` and will default to
	# the remote `origin` or can be used
	# `gclean upstream` to target specific
	# remotes
	currentbranch=$(git rev-parse --abbrev-ref HEAD)
	remote=${1:-origin}
	echo "current branch is '$currentbranch'"
	echo "pruning remote '$remote' refs that no longer"
	git remote prune $remote
	# List out the merged branches
	echo "deleting remote branches merged into '$currentbranch'"
	git branch -r --merged $currentbranch |
		# filter out everything that has the same name as the current branch
		egrep -iv "$currentbranch" |
		# grab only those of the current remote
		egrep -i "$remote/" |
		# filter out remote name from the string
		sed "s/$remote\///g" |
		# push and delete the branch
		xargs -n 1 git push --delete $remote
}
gbranchdelete() {
	# delete a branch both localy and remotely
	# gbranch delete foo/branch
	remote=${2:-origin}
	echo "branch to delete is '$1' locally and from remote $remote"
	echo "Do you want to continue?"
	select yn in "Yes" "No"; do
		case $yn in
		Yes) break ;;
		No) return 0 ;;
		esac
	done
	git push -d $remote $1
	git branch -D $1
}
fbranch() {
	# fbranch
	# search for branch using fuzzy search
	local branches branch
	branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
		branch=$(echo "$branches" | fzf-tmux -d $((2 + $(wc -l <<<"$branches"))) +m) &&
		git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}
_gbranches() {
	git --no-pager branch --all \
		--format="%(if)%(HEAD)%(then)%(else)%(if:equals=HEAD)%(refname:strip=3)%(then)%(else)%1B[0;34;1mbranch%09%1B[m%(refname:short)%(end)%(end)" |
		sed '/^$/d'
}
# TODO: make gco ./path/to/file work as well
gco() {
	# gco - checkout git branch/tag with fzf search
	local branches target
	local query=$1

	# TODO: make search through remote branches
	local existed_in_local=$(git branch --list ${query})
	if [[ -n $existed_in_local ]] || [[ -f "$query" ]] || [[ -d "$query" ]]; then
		git checkout $query
		return
	fi

	tags=$(git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return

	branches=$(_gbranches) || return

	target=$(
		(
			echo "$branches"
			echo "$tags"
		) |
			fzf-tmux -d50% -- --no-hscroll --ansi +m -d "\t" -n 2 --preview="git --no-pager log -150 --pretty=format:%s '..{2}'" -q $query
	) || return

	branch_name=$(echo "$target" | awk '{print $2}')

	# check if target starts with origin/
	# TODO: make generic to remote name
	if [[ $branch_name = origin/* ]]; then
		git checkout -b ${branch_name#"origin/"} --track $branch_name
	else
		git checkout $branch_name
	fi
}

gmerge() {
	local branches target
	local query=$1

	tags=$(git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return

	branches=$(_gbranches) || return

	target=$(
		(
			echo "$branches"
			echo "$tags"
		) |
			fzf-tmux -d50% -- --no-hscroll --ansi +m -d "\t" -n 2 --preview="git --no-pager log -150 --pretty=format:%s '..{2}'" -q $query
	) || return

	branch_name=$(echo "$target" | awk '{print $2}')

	git merge $branch_name
}
