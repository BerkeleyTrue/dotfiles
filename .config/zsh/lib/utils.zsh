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
