# Pack section data into a tuple
ghanima::sections::pack() {
  zparseopts -E -D fg:=fg_ bg:=bg_ n=newline || return

  local content="$@"
  local tuple=()

  tuple+=("(")

  # if newline is set, content is ignored
  if [[ -n "$newline" ]]; then
    tuple+=("unset")
    tuple+=("unset")
    tuple+=("newline")
  else
    tuple+=("$fg_[-1]")
    tuple+=("$bg_[-1]")
    tuple+=("$content")
  fi

  tuple+=(")")

  print -n "${(j:.|.:)tuple}"
}

# get foreground from section data
# ghanima::sections::foreground <section_data>
ghanima::sections::foreground() {
  local tuple="$1" section_data=()

  section_data=("${(@s:.|.:)tuple}")

  print -n "${section_data[2]}"
}

# get background from section data
#  ghanima::sections::background <section_data>
ghanima::sections::background() {
  local tuple="$1" section_data=()

  section_data=("${(@s:.|.:)tuple}")

  print -n "${section_data[3]}"
}

# get content from section data
#  ghanima::sections::content <section_data>
ghanima::sections::content() {
  local tuple="$1" section_data=()

  section_data=("${(@s:.|.:)tuple}")

  print -n "${section_data[4]}"
}
