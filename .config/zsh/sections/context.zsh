ghanima::sections::context() {
  local fg bg content

  if [[ "$USER" != "$LOGNAME" || -n "$SSH_CONNECTION" ]]; then
    bg=black
    fg=yellow
    content="%n@%m"
  else
    bg=blue
    fg=cyan
    content=$(ghanima::emoji delta)
  fi

  ghanima::sections::pack -fg $fg -bg $bg $content
}
