# Upsearch for specific file or directory
# Returns the path to the first found file or fails
# USAGE:
#   ghanima::upsearch <paths...>
# EXAMPLE:
#   $ ghanima::upsearch package.json node_modules
#   > /home/username/path/to/project/node_modules
ghanima::upsearch() {
  # Parse CLI options
  zparseopts -E -D \
    s=silent -silent=silent

  local files=("$@")
  local root="$(pwd -P)"

  # Go up to the root
  while [ "$root" ]; do
    # For every file as an argument
    for file in "${files[@]}"; do
      local find_match="$(fd $file $root --max-depth 1 --print0 --quit 2>/dev/null)"
      local filename="$root/$file"
      if [[ -n "$find_match" ]]; then
        [[ -z "$silent" ]] && echo "$find_match"
        return 0
      elif [[ -e "$filename" ]]; then
        [[ -z "$silent" ]] && echo "$filename"
        return 0
      fi
    done

    if [[ -d "$root/.git" ]]; then
      # If we reached the root of repo, return non-zero
      return 1
    fi

    # Go one level up
    root="${root%/*}"
  done

  # If we reached the root, return non-zero
  return 1
}

ghanima::is_inside_git_repo() {
  [[ $(command git rev-parse --is-inside-work-tree 2>/dev/null) = true ]]
}

ghanima::git_ref() {
  local ref
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref="◈ $(git describe --exact-match --tags HEAD 2> /dev/null)" || \
  ref="➦ $(git rev-parse --short HEAD 2> /dev/null)"
  echo $ref
}
ghanima::gh_user() {
  local ref
  ref=$(gh auth status | grep -m 1 'Logged in to github.com' | awk '{print $7}')
  echo $ref
}
# Check if command exists in $PATH
# USAGE:
#   ghanima::exists <command>
ghanima::exists() {
  command -v $1 >/dev/null 2>&1
}

# Check if function is defined
# USAGE:
#   ghanima::defined <function>
ghanima::defined() {
  typeset -f + "$1" &> /dev/null
}

# Precompile zsh file to ZWC (zsh word code)
# USAGE:
#  ghanima::precomile <file>
ghanima::precompile() {
  ghanima::exists zcompile || return 1

  local file="$1"
  # remove zsh extension and add zwc
  local filezwc="${file%.*}.zwc"

  if [[ ! $filezwc -nt $file && -w "$(dirname $1)" ]]; then
    zcompile -R -- $filezwc $file
  fi
}
# Union of two or more arrays
# USAGE:
#   ghanima::union [arr1[ arr2[ ...]]]
# EXAMPLE:
#   $ arr1=('a' 'b' 'c')
#   $ arr2=('b' 'c' 'd')
#   $ arr3=('c' 'd' 'e')
#   $ ghanima::union $arr1 $arr2 $arr3
#   > a b c d e
ghanima::union() {
  typeset -U sections=("$@")
  echo $sections
}

# Display seconds in human readable format
# For that use `strftime` and convert the duration (float) to seconds (integer).
# USAGE:
#   ghanima::displaytime <seconds> [precision]
ghanima::displaytime() {
  local duration="$1" precision="$2"

  [[ -z "$precision" ]] && precision=1

  integer D=$((duration/60/60/24))
  integer H=$((duration/60/60%24))
  integer M=$((duration/60%60))
  local S=$((duration%60))

  [[ $D > 0 ]] && printf '%dd ' $D
  [[ $H > 0 ]] && printf '%dh ' $H
  [[ $M > 0 ]] && printf '%dm ' $M

  printf %.${precision}f%s $S s
}
