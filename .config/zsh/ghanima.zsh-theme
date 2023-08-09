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

PLUSMINUS="󱓊 "
BRANCH="󰘬"
DETACHED="󱓌 "
BOMB=" "
LIGHTNING="󱐋"
GEAR=" "
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
    echo -n "%{%F{$CURRENT_BG}$bg%}$segment%{$fg%}"
  else
    echo -n "%{$bg%}%{$fg%}"
  fi
  CURRENT_BG=$1
  [[ -n $3 ]] && echo -n $3
}

# End:
# - End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    echo -n "%{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    echo -n "%{%k%}"
  fi
  echo -n "%{%f%} "
  CURRENT_BG=''
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
  # if previous command failed
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}$BOMB%{%f%}"
  # if root
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}$LIGHTNING%{%f%}"
  # if background jobs
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}$GEAR%{%f%}"

  [[ -n "$symbols" ]] && prompt_segment black $PRIMARY_FG " $symbols"
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
prompt_dir() {
  local ref
  local wd="$(pwd | sed -e "s,^$HOME,~,")"
  local dir="$(basename $wd)"
  local pd="$(basename $(dirname $wd))"
  local wdl="$(printf $wd | wc -c)"
  local numOfDirs="$(echo $wd | sed -e "s,~/,," | tr '/' '\n' | wc -l)"

  if [[ numOfDirs -lt 3  ]]; then
    ref=" $wd "
  else
    ref=".../$pd/$dir "
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

# start the right prompt
prompt_right_start() {
  echo -n "%{%k%F{$1}%}$RSEGMENT_SEPARATOR"
}

# Git: branch/detached head, dirty status
prompt_git() {
  local color ref repo_path is_dirty symbol mode

  if [[ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = "true" ]]; then
    is_dirty=$(git status --porcelain --ignore-submodules 2> /dev/null | tail -n 1)
    repo_path=$(git rev-parse --git-dir 2> /dev/null)

    ref=$(git symbolic-ref HEAD 2> /dev/null) || \
    ref="◈ $(git describe --exact-match --tags HEAD 2> /dev/null)" || \
    ref="➦ $(git rev-parse --short HEAD 2> /dev/null)"

    if [[ -n $is_dirty ]]; then
      color=yellow
      symbol=$PLUSMINUS
    else
      color=green
    fi

    if [[ "${ref/.../}" == "$ref" ]]; then
      symbol=$BRANCH
      ref=${ref#refs/heads/}
    else
      ref="${ref/.../} $DETACHED"
    fi

    if [[ -e "${repo_path}/BISECT_LOG" ]]; then
      mode=" <B>"
    elif [[ -e "${repo_path}/MERGE_HEAD" ]]; then
      mode=" >M<"
    elif [[ -e "${repo_path}/rebase" || -e "${repo_path}/rebase-apply" || -e "${repo_path}/rebase-merge" || -e "${repo_path}/../.dotest" ]]; then
      mode=" >R>"
    fi

    prompt_right_start $color
    IN_GIT_REPO=$color

    echo -n "%{%K{$color}%F{$PRIMARY_FG}%} $ref $symbol ${mode} "
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
  echo -n "%{%K{$bg}%F{$fg}%}$URSEGMENT_SEPARATOR"
}

# End the right prompt
prompt_right_end() {
  echo -n "%{%k%f%}"
  CURRENT_BG='NONE'
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

    echo -n "%{%K{cyan}%F{black}%} %{%B%}$name%{%b%} $NIX "
  fi
}

prompt_top_left() {
  # Set the return value of the previous command
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
  # The right prompt should be on the same line as the first line of the left
  # prompt. To do so, there is just a quite ugly workaround: Before zsh draws
  # the RPROMPT, we advise it, to go one line up. At the end of RPROMPT, we
  # advise it to go one line down. See:
  # http://superuser.com/questions/357107/zsh-right-justify-in-ps1
  local rprompt_prefix='\e[1A' # one line up
  local rprompt_suffix='\e[1B' # one line down
  # Make sure KEYMAP is initial set to insert
  # otherwise this is empty
  KEYMAP="viins"
  TOP_PROMPT='%{%f%b%k%}$(prompt_top_left)'
  PROMPT=$TOP_PROMPT$'\n''$(prompt_bottom_left)'

  RPROMPT='%{%f%b%k%}%{${rprompt_prefix}%}$(prompt_bottom_right)%{${rprompt_suffix}%}'
}

prompt_ghanima_setup() {
  autoload -Uz add-zsh-hook

  # This variable is a magic variable used when loading themes with zsh's prompt
  # function. It will ensure the proper prompt options are set.
  prompt_opts=(cr subst percent)

  # Borrowed from promptinit, sets the prompt options in case the prompt was not
  # initialized via promptinit.
  setopt noprompt{bang,cr,percent,subst} "prompt${^prompt_opts[@]}"

  add-zsh-hook precmd prompt_ghanima_precmd
}

prompt_ghanima_setup "$@"

# vim:ft=zsh
