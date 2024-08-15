ghanima::sections::git() {
  local color ref repo_path is_dirty symbol mode user

  if ghanima::is_inside_git_repo; then
    is_dirty=$(git status --porcelain --ignore-submodules 2> /dev/null | tail -n 1)
    repo_path=$(git rev-parse --git-dir 2> /dev/null)
    ref=$(ghanima::git_ref)
    user=$(ghanima::gh_user | tr '[:upper:]' '[:lower:]')

    if [[ -n $is_dirty ]]; then
      color=yellow
      symbol=$(ghanima::emoji plusminus)
    else
      color=green
    fi

    if [[ "${ref/.../}" == "$ref" ]]; then
      symbol=$(ghanima::emoji branch)
      ref=${ref#refs/heads/}
    else
      symbol=$(ghanima::emoji detached)
    fi

    if [[ -e "${repo_path}/BISECT_LOG" ]]; then
      mode=" <B>"
    elif [[ -e "${repo_path}/MERGE_HEAD" ]]; then
      mode=" >M<"
    elif [[ -e "${repo_path}/rebase" || -e "${repo_path}/rebase-apply" || -e "${repo_path}/rebase-merge" || -e "${repo_path}/../.dotest" ]]; then
      mode=" >R>"
    fi

    ghanima::sections::pack -fg black -bg $color "$user@%{%B%}$ref%{%b%} $symbol ${mode}"
  fi
}
