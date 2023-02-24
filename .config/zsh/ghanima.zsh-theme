### Segment drawing
# A few utility functions to make it easy and re-usable to draw segmented prompts

CURRENT_BG='NONE'
if [[ -z "$PRIMARY_FG" ]]; then
  PRIMARY_FG=black
fi

# Used by vim plugin
MODE_INDICATOR=""
IN_GIT_REPO=""

# Characters
SEGMENT_SEPARATOR=""
USEGMENT_SEPARATOR=""

RSEGMENT_SEPARATOR=""
URSEGMENT_SEPARATOR=""

PLUSMINUS="\u00b1"
BRANCH="\ue0a0"
DETACHED="\u27a6"
CROSS="\u2718"
BOMB=" "
LIGHTNING="\u26a1"
GEAR="\u2699"
DELTA=" "
NIX=" "
NODE="󰎙"

SEGMENT_NUM=0

# F set foreground
# f reset foreground
# K set background
# k reset background


# Begin a segment
# prompt_segment bg fg content
prompt_segment() {
  local bg fg segment
  # when empty strings, these revert the custom color
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
  #vary segment vertical
  [[ $((SEGMENT_NUM % 2 == 0)) -gt 0 ]] && segment=$SEGMENT_SEPARATOR || segment=$USEGMENT_SEPARATOR
  # increment segment number
  SEGMENT_NUM=$((SEGMENT_NUM+1))
  # if not the first segment and
  # current background is not request background
  # this is to make the next separator's color (a foreground color)
  # in line with the previous background
  if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
    # start using new background
    # start using stored previous bg as new foreground
    # prefix separator
    # start using fg color
    print -n "%{%F{$CURRENT_BG}$bg%}$segment%{$fg%}"
  else
    print -n "%{$bg%}%{$fg%}"
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && print -n $3
}

### Prompt components
# Each component will draw itself, and hide itself if no information needs to be shown

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}$BOMB%{%f%}"
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}$LIGHTNING%{%f%}"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}$GEAR%{%f%}"

  [[ -n "$symbols" ]] && prompt_segment '239' $PRIMARY_FG " $symbols"
}

# Context:
# - user@hostname (who am I and where am I)
prompt_context() {
  local user=`whoami`

  if [[ "$user" != "$DEFAULT_USER" || -n "$SSH_CONNECTION" ]]; then
    prompt_segment $PRIMARY_FG default " %(!.%{%F{yellow}%}.)$user@%m"
  else
    prompt_segment blue cyan " %(!.%{%F{red}%}.)$DELTA"
  fi
}

# Dir:
# - current working directory
# TODO: figure out .config bug
prompt_dir() {
  local ref
  local wd="$(pwd | sed -e "s,^$HOME,~,")"
  local dir="$(basename $wd)"
  local pd="$(basename $(dirname $wd))"
  local wdl="$(printf $wd | wc -c)"
  if [[ wdl -lt 14  ]]; then
    ref=" $wd "
  else
    ref="../$pd/$dir "
  fi

  prompt_segment black green $ref
}

# VIM:
# - show current mode
prompt_vim() {
  local fgr bk ref
  if [[ $KEYMAP = 'viins' ]] || [[ $KEYMAP = 'main' ]]; then
    bk=cyan
    fgr=black
    ref=" %{%B%}INSERT %{%b%}"
  elif [[ $KEYMAP = 'vicmd' ]]; then
    bk=blue
    fgr=white
    ref=" %{%B%}NORMAL %{%b%}"
  elif [[ $KEYMAP = 'viopp' ]]; then
    bk=cyan
    fgr=black
    ref=" %{%B%}OPPER %{%b%}"
  else
    bk=red
    ref=" $KEYMAP "
  fi
  prompt_segment $bk $fgr $ref
}

# End:
# - End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    print -n "%{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    print -n "%{%k%}"
  fi
  print -n "%{%f%} "
  CURRENT_BG=''
}

# start the right prompt
prompt_right_start() {
  print -n "%{%k%F{$1}%}$RSEGMENT_SEPARATOR"
}

prompt_right_start() {
  print -n "%{%k%F{$1}%}$RSEGMENT_SEPARATOR"
}
# Git: branch/detached head, dirty status
prompt_git() {
  local color ref
  is_dirty() {
    test -n "$(git status --porcelain --ignore-submodules)"
  }
  ref="$vcs_info_msg_0_"
  if [[ -n "$ref" ]]; then
    if is_dirty; then
      color=yellow
      ref="$PLUSMINUS ${ref}"
    else
      color=green
      ref="${ref}"
    fi
    if [[ "${ref/.../}" == "$ref" ]]; then
      ref="$ref $BRANCH"
    else
      ref="${ref/.../} $DETACHED"
    fi

    prompt_right_start $color
    IN_GIT_REPO=$color

    print -n "%{%K{$color}%F{$PRIMARY_FG}%}$ref "
  fi
}

# Adds a separator between right side segments
# takes as an argument the current background color
# so that the separator background matches the previous segment
# takes as a second argument the background color of the next segment
# so that the separator foreground matches the next segment
prompt_right_sep() {
  local bg fg
  bg=$1
  fg=$2
  print -n "%{%K{$bg}%F{$fg}%}$URSEGMENT_SEPARATOR"
}

# If IN_NIX_SHELL is set, show the name of the current nix-shell
prompt_nix_shell() {
  local name
  if [[ -n "$IN_NIX_SHELL" ]]; then
    if [[ -n "$NIX_SHELL_NAME" ]]; then
      name="$NIX_SHELL_NAME"
    else
      name=$IN_NIX_SHELL
    fi

    if [[ -n "$IN_GIT_REPO" ]]; then
      prompt_right_sep $IN_GIT_REPO "cyan"
    else
      prompt_right_start "cyan"
    fi

    print -n "%{%K{cyan}%F{black}%} %{%B%}$name%{%b%} $NIX "

  fi
}

prompt_right_end() {
  print -n "%{%k%f%}"
  CURRENT_BG=''
}

prompt_top_left() {
  RETVAL=$?
  CURRENT_BG='NONE'
  prompt_status
  prompt_vim
  prompt_dir
  prompt_end
}

prompt_bottom_left() {
  CURRENT_BG='NONE'
  prompt_context
  prompt_end
}

prompt_bottom_right() {
  CURRENT_BG='NONE'
  prompt_git
  prompt_nix_shell
  prompt_right_end
}

prompt_ghanima_precmd() {
  vcs_info
  # Make sure KEYMAP is initial set to insert
  # otherwise this is empty
  KEYMAP="viins"
  TOP_PROMPT='%{%f%b%k%}$(prompt_top_left)'
  PROMPT=$TOP_PROMPT$'\n''$(prompt_bottom_left)'

  RPROMPT='%{%f%b%k%}$(prompt_bottom_right)'
}

prompt_ghanima_setup() {
  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  prompt_opts=(cr subst percent)

  add-zsh-hook precmd prompt_ghanima_precmd

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' check-for-changes false
  zstyle ':vcs_info:git*' formats '%b'
  zstyle ':vcs_info:git*' actionformats '%b (%a)'
}

prompt_ghanima_setup "$@"

# vim:ft=zsh
