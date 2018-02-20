# Add vi mappings to bash
set -o vi

[[ -s "$HOME/.config/shell/index.sh"  ]] && source "$HOME/.config/shell/index.sh"
# add fzf completion
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# bash completion w/ goodies
# brew install bash-completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# Git Completion to shortcuts
if [ -n "$(type -t __git_complete)" ] && [ "$(type -t __git_complete)" = function ]; then
  __git_complete gco _git_checkout
  __git_complete grebase _git_checkout
  # will autocomplete remotes
  __git_complete gpull __git_complete_remote_or_refspec
  __git_complete gclean __git_complete_remote_or_refspec
fi
# Fix <C-h> issue with neovim
# Not sure if this is sage to run more than once
# infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti && tic $TERM.ti

###-begin-npm-completion-###
#
# npm command completion script
if type complete &>/dev/null; then
  _npm_completion () {
    local words cword
    if type _get_comp_words_by_ref &>/dev/null; then
      _get_comp_words_by_ref -n = -n @ -w words -i cword
    else
      cword="$COMP_CWORD"
      words=("${COMP_WORDS[@]}")
    fi

    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${words[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -o default -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    local si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###
