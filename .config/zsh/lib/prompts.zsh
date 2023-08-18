# echo prompt
ghanima::prompts::prompt() {
  print -n "$(ghanima::core::compose $GHANIMA_PROMPT_ORDER)"
}

# echo rprompt
ghanima::prompts::rprompt() {
  # Compose prompt from the order
  local rprompt="$(ghanima::core::rcompose $GHANIMA_RPROMPT_ORDER)"

  # The right prompt should be on the same line as the first line of the left
  # prompt. To do so, there is just a quite ugly workaround: Before zsh draws
  # the RPROMPT, we advise it, to go one line up. At the end of RPROMPT, we
  # advise it to go one line down. See:
  # http://superuser.com/questions/357107/zsh-right-justify-in-ps1
  local rprompt_prefix='%{'$'\e[1A''%}' # one line up
  local rprompt_suffix='%{'$'\e[1B''%}' # one line down
  rprompt="$rprompt_prefix$rprompt$rprompt_suffix"

  # Print the rprompt
  print -n "$rprompt"
}

ghanima::prompts::populate() {
  PROMPT='$(ghanima::prompts::prompt)'
  RPROMPT='$(ghanima::prompts::rprompt)'
}

ghanima::prompts::populate-postexec() {
  local SHORT_PROMPT="%F{cyan}=<<%f%F{blue}%*%f%F{cyan}>=>%f"

  if ghanima::is_inside_git_repo; then
    local git_content=$(ghanima::sections::content "$(ghanima::cache::get "git")")
    SHORT_PROMPT="%F{cyan}=<<%f%F{blue}%* %f%F{green} $git_content %f%F{cyan}>=>%f"
  fi

  if [[ $PROMPT != $SHORT_PROMPT ]]; then
    PROMPT=$SHORT_PROMPT
    if [[ $RPROMPT != "" ]]; then
      RPROMPT=""
    fi
    zle .reset-prompt
  fi
}

# change cursor shape based on vim mode
ghanima::prompts::update-vim-cursor() {
  if [ $KEYMAP = vicmd ]; then
    # the command mode for vi
    print -n $(ghanima::emoji block)
  else
    # the insert mode for vi
    print -n $(ghanima::emoji beam)
  fi

  # update cache
  ghanima::core::refresh_section_cache "vi-mode"
  zle .reset-prompt && zle -R
}
