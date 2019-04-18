#! /bin/bash

#git aliases
alias gstat='git status'
alias tstat='tig status'
alias gfetch='git fetch && git status'
alias gpush='git push'
alias gpushntrack='git push -u'
alias gadd='git add'
alias gdiff='tdiff'
alias gcom='git cz'
alias gamend='git commit --amend'
alias glog='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
alias glognum='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
alias gco='git checkout'
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

# yadm aliases
# +++++++++++++++++++++++++++++++++++++++++++++{{{
alias yadd='yadm add'
alias yamend='yadm commit --amend'
alias yco='yadm checkout'
alias ycom='yadm cz'
alias ydiff='yadm diff'
alias yfetch='yadm fetch'
alias ylog='yadm log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
alias ypull='yadm pull --rebase'
alias ypush='yadm push'
alias ystat='yadm status';

alias yrst='yadm reset'
alias yrst1='yadm reset HEAD^'
alias yrst2='yadm reset HEAD^^'
alias yrsth='yadm reset --hard'
alias yrsth1='yadm reset --hard HEAD^'
alias yrsth2='yadm reset --hard HEAD^^'
alias yrsts='yadm reset --soft'
alias yrsts1='yadm reset --soft HEAD^'
alias yrsts2='yadm reset --soft HEAD^^'
#}}}

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
      Yes ) break;;
      No ) return 0;;
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
      Yes ) break;;
      No ) return 0;;
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
  if [ $# == 0 ]; then
    echo "printing remote repositories"
    git remote -v
    return 0
  fi
  while test $# -gt 0; do
    case "$1" in
      -h|--help)
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
        if [[ -z "${2//}" || "${#2}" -lt 3 ]]; then
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
      *)
        echo "gremote: $1 is not a proper command"
        return 2
        ;;
    esac
  done
}
grebase() {
  # grebase
  if [ $# == 0 ]; then
    echo "grebase: Incorrect number of args"
    return 1
  fi

  if [ $1 == 'cont' ]; then
    echo "continuing..."
    git rebase --continue
    return 0
  fi

  if [ $1 == 'skip' ]; then
    echo "skiping..."
    git rebase --skip
    return 0
  fi

  if [ $1 == 'abort' ]; then
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
gpull() {
  # If given gpull upstream
  # find the current branch and pull that branch
  # from the `upstream` remote repository
  if [ $# = 1 ]; then
    currentbranch=$(ggetcurrentbranch)
    echo "current branch is $currentbranch"
    echo "pulling down branch '$currentbranch' from remote repository '$1'"
    git pull $1 $currentbranch
    return 0
  fi
  # Otherwise pull from the repository this branch is tracking
  # always do a rebase pull
  git pull --rebase
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
  git branch -r --merged $currentbranch |\
    # filter out everything that has the same name as the current branch
    egrep -iv "$currentbranch" |\
    # grab only those of the current remote
    egrep -i "$remote/" |\
    # filter out remote name from the string
    sed "s/$remote\///g" |\
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
      Yes ) break;;
      No ) return 0;;
    esac
  done
  git push -d $remote $1
  git branch -D $1
}

# git and tig/fzf
tdiff() {
  if [ $# = 0 ]; then
    git diff | tig
    return 0
  fi
  git diff $1 | tig
}
fbranch() {
  # fbranch
  # search for branch using fuzzy search
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
    branch=$(echo "$branches" | fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
    git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}
fco() {
  # fco - checkout git branch/tag with fzf search
  local tags branches target
  tags=$(
  git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
  git branch --all | grep -v HEAD             |
  sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
  sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
  (echo "$tags"; echo "$branches") |
  fzf-tmux -l30 -- --no-hscroll --ansi +m -d "\t" -n 2) || return
  git checkout $(echo "$target" | awk '{print $2}')
}
fshow() {
  # fshow - git commit browser
  git log --graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
    fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute: (grep -o '[a-f0-9]\{7\}' | head -1 | xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF' {} FZF-EOF"
}
fcs() {
  # fcs - get git commit sha
  # example usage: git rebase -i `fcs`
  local commits commit
  commits=$(git log --color=always --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}
fstash() {
  # fstash - easier way to deal with stashes
  # type fstash to get a list of your stashes
  # enter shows you the contents of the stash
  # ctrl-d shows a diff of the stash against your current HEAD
  # ctrl-b checks the stash out as a branch, for easier merging
  local i arr out q k sha
  while out=$(
    git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" |
    fzf --ansi --no-sort --query="$q" --print-query \
        --expect=ctrl-d,ctrl-b);
  do
    # below is equivalent to 'mapfile -t out <<< "$out"'
    i=0
    while IFS=$'\n' read -r line
    do
      arr[i]="$line"
      i=$((i + 1))
    done <<< "$out"

    q="${arr[0]}"
    k="${arr[1]}"
    sha="${arr[ ${#arr[@]} - 1]}"
    sha="${sha%% *}"
    [[ -z "$sha" ]] && continue
    if [[ "$k" == 'ctrl-d' ]]; then
      git diff $sha
    elif [[ "$k" == 'ctrl-b' ]]; then
      git stash branch "stash-$sha" $sha
      break;
    else
      git stash show -p $sha
    fi
  done
}
