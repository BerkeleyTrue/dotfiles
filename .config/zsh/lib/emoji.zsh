ghanima::emoji() {
  local name="$1"

  if [[ $name == "nix" ]] then
    echo " "
  elif [[ $name == "nodejs" ]] then
    echo "󰎙"
  elif [[ $name == "div" ]] then
    echo ""
  elif [[ $name == "divi" ]] then
    echo ""
  elif [[ $name == "rdiv" ]] then
    echo ""
  elif [[ $name == "rdivi" ]] then
    echo ""
  elif [[ $name == "plusminus" ]] then
    echo "󱓊 "
  elif [[ $name == "branch" ]] then
    echo "󰘬"
  elif [[ $name == "detached" ]] then
    echo "󱓌 "
  elif [[ $name == "bomb" ]] then
    echo " "
  elif [[ $name == "lightning" ]] then
    echo "󱐋"
  elif [[ $name == "gear" ]] then
    echo " "
  elif [[ $name == "delta" ]] then
    echo "󰚌 "
  else
    echo ""
  fi
}
