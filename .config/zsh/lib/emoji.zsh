ghanima::emoji() {
  local name="$1"

  # wrap in %1{} to ensure single width in zsh prompt
  # otherwise it renders weirdly with in wayland
  if [[ $name == "nix" ]] then
    echo "%1{%}"
  elif [[ $name == "nodejs" ]] then
    echo "%1{󰎙%}"
  elif [[ $name == "div" ]] then
    echo "%1{%}"
  elif [[ $name == "divi" ]] then
    echo "%1{%}"
  elif [[ $name == "rdiv" ]] then
    echo "%1{%}"
  elif [[ $name == "rdivi" ]] then
    echo "%1{%}"
  elif [[ $name == "cir" ]] then
    echo "%1{%}"
  elif [[ $name == "rcir" ]] then
    echo "%1{%}"
  elif [[ $name == "plusminus" ]] then
    echo "%1{󱓊%}"
  elif [[ $name == "branch" ]] then
    echo "%1{󰘬%}"
  elif [[ $name == "detached" ]] then
    echo "%1{󱓌%}"
  elif [[ $name == "bomb" ]] then
    echo "%1{%}"
  elif [[ $name == "lightning" ]] then
    echo "%1{󱐋%}"
  elif [[ $name == "gear" ]] then
    echo "%1{%}"
  elif [[ $name == "delta" ]] then
    echo "%1{󰚌%}"
  elif [[ $name == "block" ]] then
    echo "\e[1 q"
  elif [[ $name == "beam" ]] then
    echo "\e[5 q"
  else
    echo ""
  fi
}
