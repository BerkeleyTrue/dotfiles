ghanima::hooks::preexec() {
  GHANIMA_EXEC_TIME_start=$EPOCHREALTIME
}

ghanima::hooks::exec-time-stop() {
  [[ -n $GHANIMA_EXEC_TIME_duration ]] && unset GHANIMA_EXEC_TIME_duration
  [[ -z $GHANIMA_EXEC_TIME_start ]] && return

  GHANIMA_EXEC_TIME_duration=$((EPOCHREALTIME - GHANIMA_EXEC_TIME_start))

  # Reset start time
  unset GHANIMA_EXEC_TIME_start
}

# A hook before every command
ghanima::hooks::precmd() {
  # Set the return value of the previous command
  RETVAL=$?

  # Stop the timer
  ghanima::hooks::exec-time-stop

  # Start building cache from sections
  ghanima::core::start

  # Initiate the first render
  ghanima::prompts::populate
}

ghanima::hooks::line-finish() {
  ghanima::prompts::populate-postexec
}

ghanima::hooks::line-init() {
  ghanima::prompts::update-vim-cursor
}

ghanima::hooks::zle-keymap-select() {
  ghanima::prompts::update-vim-cursor
}
