ghanima::sections::nix() {
  local nixname

  # not in nix shell
  [[ -z "$IN_NIX_SHELL" ]] && return

  if [[ -z "$name" || "$name" == "" ]] then
    nixname="$NIX_SHELL_NAME"
  else
    nixname="$name"
  fi

  ghanima::sections::pack -fg black -bg cyan "%{%B%}$nixname%{%b%} $(ghanima::emoji nix) "
}
