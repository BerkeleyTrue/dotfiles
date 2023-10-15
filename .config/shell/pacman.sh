#!/bin/bash

alias pupdate='sudo pacman -Syu'
alias pin='sudo pacman -S'
alias pinfo='pacman -Si'

alias pun='sudo pacman -Rs'
alias punorphans='sudo pacman -Rs $(pacman -Qtdq)'

alias pls='pacman -Qe'
alias plsorphans='sudo pacman -Qtd'

alias yayupdate='yay -Syu'

function pacweb() {
  if [[ $# = 0 || "$1" =~ '--help|-h' ]]; then
    local underline_color="\e[${color[underline]}m"
    echo "$0 - open the website of an ArchLinux package"
    echo
    echo "Usage:"
    echo "    $bold_color$0$reset_color ${underline_color}target${reset_color}"
    return 1
  fi

  local pkg="$1"
  local infos="$(LANG=C pacman -Si "$pkg")"
  if [[ -z "$infos" ]]; then
    return
  fi
  local repo="$(grep -m 1 '^Repo' <<<"$infos" | grep -oP '[^ ]+$')"
  local arch="$(grep -m 1 '^Arch' <<<"$infos" | grep -oP '[^ ]+$')"
  xdg-open "https://www.archlinux.org/packages/$repo/$arch/$pkg/" &>/dev/null
}

function paclist() {
  # Based on https://bbs.archlinux.org/viewtopic.php?id=93683
  pacman -Qqe |
    xargs -I '{}' \
      expac "${bold_color}% 20n ${fg_no_bold[white]}%d${reset_color}" '{}'
}
