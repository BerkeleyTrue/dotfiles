#! /bin/sh
alias npmig='sudo npm install -g'
alias npmug='sudo npm uninstall -g'

alias npmid='npm install --save-dev'
alias npmud='npm uninstall --save-dev'

alias npmis='npm install --save'
alias npmus='npm uninstall --save'

alias npmlsg='sudo npm list -g --depth=0'

###
# install npm prerequisites:
###
# `npmpublish` with optional argument `patch`/`minor`/`major`/`<version>`
# defaults to conventional-recommended-bump
# and optional argument preset `angular`/ `jquery` ...
# defaults to conventional-commits-detector
npmpublish() {
  # travis status --no-interactive &&
  # start with fresh npm install
  npx trash node_modules &>/dev/null
  # should almost always be angular
  preset=${2:-$(npx conventional-commits-detector)}
  bump=${1:-$(npx conventional-recommended-bump -p $preset)}
  if [ -z $CONVENTIONAL_GITHUB_RELEASER_TOKEN ]; then
    echo "CONVENTIONAL_GITHUB_RELEASER_TOKEN not found"
    return 1
  fi
  echo "Bump: $bump."
  echo "Preset: $preset"
  echo "Do you wish to continue?"
  select yn in "Yes" "No"; do
    case $yn in
    Yes) break ;;
    No) return 0 ;;
    esac
  done
  # make sure we have the latest commits
  git pull --rebase &&
    # get fresh packages
    npm ci &&
    # copy package.json for later restoration
    cp package.json _package.json &&
    # copy package-lock if exists
    [[ -e package-lock.json ]] && cp package-lock.json _package-lock.json || echo "info: no package-lock found" &&
    npm --no-git-tag-version version $bump &>/dev/null &&
    echo "creating/updating changelong" &&
    npx conventional-changelog -i CHANGELOG.md -s -p $preset &&
    git add CHANGELOG.md &&
    version=$(cat package.json | json version) &&
    echo "Adding changelog" &&
    git commit -m "docs(CHANGELOG): v$version" &&
    mv -f _package.json package.json &&
    [[ -e _package-lock.json ]] && mv -f _package-lock.json package-lock.json ||
    echo "cutting release" &&
    npm version $bump -m "chore(release): v%s" &&
    echo "pushing up changes" &&
    git push --follow-tags &&
    echo "creating github release" &&
    npx conventional-github-releaser -p $preset &&
    echo "publishing to npm" &&
    npm publish
}

npmrs() {
  local script=$(cat package.json | json scripts | json -k | json -ga | fzf -m --print0 --preview="cat package.json | json scripts.{} | bat --language bash --color always" --height 40%)
  if [ -z "$script" ]; then
    return 0
  fi
  command="npm run $script"

  echo $command | clipboard
  echo "Running '$command'"
  eval $command
}
