ghanima::sections::dir() {
  local ref
  local wd="$(pwd | sed -e "s,^$HOME,~,")"
  local dir="$(basename $wd)"
  local pd="$(basename $(dirname $wd))"
  local wdl="$(printf $wd | wc -c)"
  local numOfDirs="$(echo $wd | sed -e "s,~/,," | tr '/' '\n' | wc -l)"

  if [[ numOfDirs -lt 3  ]]; then
    ref=" $wd "
  else
    ref=".../$pd/$dir "
  fi

  ghanima::sections::pack -fg green -bg black $ref
}
