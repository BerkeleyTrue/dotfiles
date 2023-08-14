ghanima::sections::status() {
  local symbols=()
  symbols=()
  # if previous command failed
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}$(ghanima::emoji bomb)%{%f%}"
  # if root
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}$(ghanima::emoji lightning)%{%f%}"
  # if background jobs
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}$(ghanima::emoji gear)%{%f%}"

  [[ -n "$symbols" ]] && ghanima::sections::pack -fg white -bg black "$symbols"
}
