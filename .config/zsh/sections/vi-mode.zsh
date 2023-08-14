ghanima::sections::vi-mode() {
  local fgr bk ref

  case "${KEYMAP:-viins}" in
    vicmd)
      case "${REGION_ACTIVE}" in
        1)
          bk=yellow
          fgr=black
          ref="VISUAL"
          ;;
        2)
          bk=yellow
          fgr=black
          ref="V-LINE"
          ;;
        *)
          bk=blue
          fgr=black
          ref="NORMAL"
          ;;
      esac
      ;;

    main|viins)
      ref="INSERT"
      if [[ "${ZLE_STATE}" == *overwrite* ]]; then
        bk=red
        fgr=black
        ref="REPLACE"
      else
        bk=green
        fgr=black
        ref="INSERT"
      fi
      ;;


    viopp)
      bk=cyan
      fgr=black
      ref="OPPER"
      ;;

    *)
      bk=red
      fgr=white

      if [[ -z $KEYMAP ]]; then
        ref="EMPTY"
      else
        ref="UNKNOWN"
      fi

      ;;
  esac

  ghanima::sections::pack -fg $fgr -bg $bk "%{%B%}$ref%{%b%}"
}
