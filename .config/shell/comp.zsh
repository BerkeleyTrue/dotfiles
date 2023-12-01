# Define the completion function for gwta
_gwta.gbranches() {
  local typed_branch=$1

  if [ -z "$typed_branch" ]; then
    git branch --list | sed 's/\s\|\*//g'
  else
    git branch --list "$typed_branch*" | sed 's/\s\|\*//g'
  fi
}

_gwta() {
  local -a branches remotes
  local typed_branch=${words[$CURRENT]}
  local is_git_branch=$(git rev-parse --is-inside-work-tree &>/dev/null && git rev-parse --is-bare-repository &>/dev/null)

  # Define the completion options
  _arguments \
    '1: :->path' \
    '2: :->branchname' \
    '3: :->git_branch' \
    '4: :->git_remote'

  case "$state" in
    path)
      # Complete the path
      _files -/\'
      ;;
    branchname)
      if [ -b $is_git_branch ]; then
        branches=($(_gwta.gbranches $typed_branch))
        # Complete local branch names
        _describe 'branches' branches
      else
        # Complete the path
        _files -/\'
      fi
      ;;
    git_branch)
      if [ -b $is_git_branch ]; then
        branches=($(_gwta.gbranches $typed_branch))
        # Complete local branch names
        _describe 'branches' branches
      else
        # Complete the path
        _files -/\'
      fi
      ;;
    git_remote)
      remotes=($(git remote))
      # Complete remote names
      _describe 'remotes' remotes
      ;;
  esac
}

# Register the completion function for gwta
compdef _gwta gwta
