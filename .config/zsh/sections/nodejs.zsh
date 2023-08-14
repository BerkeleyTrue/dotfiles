ghanima::sections::nodejs() {
  local is_nodejs_project=$(ghanima::upsearch package.json node_versions)
  [[ -n "$is_nodejs_project" ]] || return

  local node_version

  if ghanima::exists node; then
    node_version=$(node --version)
  else
    return
  fi

  [[ $node_version == "system" || $node_version == "node" ]] && return

  ghanima::sections::pack -fg black -bg green  "%{%B%} $node_version $(ghanima::emoji nodejs) %{%b%}"
}
