GHANIMA_EXEC_TIME_ELAPSED=2
GHANIMA_EXEC_TIME_PRECISION=1

ghanima::sections::exec_time() {
  local fg bg
  local dur=$GHANIMA_EXEC_TIME_duration

  if (( dur >= GHANIMA_EXEC_TIME_ELAPSED )); then
    local content=$(ghanima::displaytime $GHANIMA_EXEC_TIME_duration $GHANIMA_EXEC_TIME_PRECISION)
    if (( dur >= 8 )); then
      fg=red
      content="%{%B%}$content%{%b%}"
    elif (( dur >= 4 )); then
      fg=magenta
    else
      fg=black
    fi

    ghanima::sections::pack -fg $fg -bg "#808080"  " $content "
  fi
}
