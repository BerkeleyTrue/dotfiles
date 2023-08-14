ghanima::core::end_prompt() {
  local current_bg=$1

  if [[ -n $current_bg ]]; then
    print -n "%{%k%F{$current_bg}%}$(ghanima::emoji div)"
  else
    print -n "%{%k%}"
  fi

  print -n "%{%f%}"

}
# compose prompt from list of sections
# ghanima::core::compose section1 section2 section3
ghanima::core::compose() {
  local segment_data next_fg next_bg
  local segment_number=0
  local current_bg="NONE"
  local separator=""

  for section in $@; do
    segment_data="$(ghanima::cache::get $section)"

    content=$(ghanima::sections::content $segment_data)
    next_fg=$(ghanima::sections::foreground $segment_data)
    next_bg=$(ghanima::sections::background $segment_data)

    [[ -z "$content" ]] && continue

    #vary segment separator
    [[ $((segment_number % 2 == 0)) -gt 0 ]] \
      && separator=$(ghanima::emoji div) \
      || separator=$(ghanima::emoji divi)

    # increment segment number
    segment_number=$((segment_number+1))


    # newline will have unset for fg and bg
    if [[ $next_fg == "unset" || $next_bg == "unset" ]]; then
      ghanima::core::end_prompt $current_bg
      next_bg="NONE"
      content=$'\n'

    # if not the first segment and
    # current background is not request background
    # this is to make the next separator's color (a foreground color)
    # in line with the previous background
    elif [[ $current_bg != 'NONE' ]] && [[ $current_bg != $next_bg ]]; then
      # start using stored previous bg as new foreground
      # start using new background
      # prefix separator
      # start using fg color
      print -n " %{%F{$current_bg}%K{$next_bg}%}$separator%{%F{$next_fg}%}"
    else
      print -n "%{%F{$next_fg}%K{$next_bg}%} "
    fi

    current_bg=$next_bg

    print -n "$content"
  done

  ghanima::core::end_prompt $current_bg

  # add some padding between prompt and cursor
  print -n " "
}

# compose rprompt from list of sections
# ghanima::core::rcompose section1 section2 section3
ghanima::core::rcompose() {
  local segment_data next_fg next_bg
  local segment_number=0
  local current_bg="NONE"
  local separator=""

  for section in $@; do
    segment_data="$(ghanima::cache::get $section)"

    content=$(ghanima::sections::content $segment_data)
    next_fg=$(ghanima::sections::foreground $segment_data)
    next_bg=$(ghanima::sections::background $segment_data)

    [[ -z "$content" ]] && continue

    # rprompt ignores newline
    [[ $content == "newline" ]] && continue

    #vary segment separator
    [[ $((segment_number % 2 == 0)) -gt 0 ]] \
      && separator=$(ghanima::emoji rdiv) \
      || separator=$(ghanima::emoji rdivi)

    # increment segment number
    segment_number=$((segment_number+1))


    # if not the first segment and
    # current background is not request background
    # this is to make the next separator's color (a foreground color)
    # in line with the previous background
    if [[ $current_bg != 'NONE' ]] && [[ $current_bg != $next_bg ]]; then
      # start using stored previous bg as new foreground
      # start using new background
      # prefix separator
      # start using fg color
      print -n "%{%F{$next_bg}%K{$current_bg}%}$separator%{%F{$next_fg}%K{$next_bg}%} "
    else
      print -n "%{%F{$next_bg}%k%}$separator%{%F{$next_fg}%K{$next_bg}%} "
    fi

    current_bg=$next_bg

    print -n "$content"
  done

  print -n "%{%k%f%}"
}

# render and reset the prompt
ghanima::core::render() {
  ghanima::prompts::populate

  zle && zle .reset-prompt && zle -R
}

# add a warning if section not found
ghanima::core::skip_section() {
  local section="$1"
  print -P "%F{yellow}Warning!%f The '%F{cyan}${section}%f' section was not found."
  GHANIMA_PROMPT_ORDER=("${(@)GHANIMA_PROMPT_ORDER:#${section}}")
}

# load and precompile sections
ghanima::core::load() {
  local force=$1
  for section in $(ghanima::union $GHANIMA_PROMPT_ORDER $GHANIMA_RPROMPT_ORDER); do
    if [[ -z "$force" ]] && ghanima::defined "ghanima::sections::$section"; then
      # echo "already defined"
      continue
    elif [[ -f "$GHANIMA_ROOT/sections/$section.zsh" ]]; then
      # echo "sourcing and compiling $section"
      builtin source "$GHANIMA_ROOT/sections/$section.zsh"
      ghanima::precompile "$GHANIMA_ROOT/sections/$section.zsh"
    else
      # echo "not found"
      ghanima::core::skip_section "$section"
      continue
    fi
  done
}

# refresh the section cache
ghanima::core::refresh_section_cache() {
  local section=$1
  [[ -z "$section" ]] && return 1

  if ! ghanima::defined "ghanima::sections::$section"; then
    ghanima::core::skip_section "$section"
    return 1
  fi

  ghanima::cache::set "$section" "$(ghanima::sections::$section)"
}

# Iterate over sections and store results in cache
# USAGE:
#   ghanima::core::start
ghanima::core::start() {
  # Clear the cache before every render
  ghanima::cache::clear

  for section in $(ghanima::union $GHANIMA_PROMPT_ORDER $GHANIMA_RPROMPT_ORDER); do
    ghanima::core::refresh_section_cache "$section"
  done
}
