ghanima::sections::vi-mode() {
  local fgr bk ref

  case "${ZVM_MODE:-$ZVM_MODE_INSERT}" in
    $ZVM_MODE_NORMAL)
      bk=blue
      fgr=black
      ref="NORMAL"
      ;;
    $ZVM_MODE_VISUAL)
      bk=yellow
      fgr=black
      ref="VISUAL"
      ;;
    $ZVM_MODE_VISUAL_LINE)
      bk=yellow
      fgr=black
      ref="V-LINE"
      ;;
    $ZVM_MODE_INSERT)
      ref="INSERT"
      bk=green
      fgr=black
      ;;
    $ZVM_MODE_REPLACE)
      bk=red
      fgr=black
      ref="REPLACE"
      ;;
    *)
      bk=red
      fgr=white
      ref="UNKNOWN"
      ;;
  esac

  ghanima::sections::pack -fg $fgr -bg $bk "%{%B%}$ref%{%b%}"
}
