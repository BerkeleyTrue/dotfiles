#! /bin/bash
###
# install npm prerequisites:
###
npminstallpre() {
  sudo npm install -g \
    trash \
    commitizen \
    cz-freecodecamp \
    conventional-changelog-angular \
    conventional-recommended-bump \
    conventional-changelog-cli \
    conventional-github-releaser \
    conventional-commits-detector \
    eslint \
    eslint-plugin-react \
    eslint-plugin-import \
    eslint-plugin-prefer-object-spread \
    eslint-plugin-jest \
    eslint-config-freecodecamp \
    eslint-plugin-rulesdir \
    babel-eslint \
    json
}
# `npmpublish` with optional argument `patch`/`minor`/`major`/`<version>`
# defaults to conventional-recommended-bump
# and optional argument preset `angular`/ `jquery` ...
# defaults to conventional-commits-detector
npmpublish() {
  # travis status --no-interactive &&
  # start with fresh npm install
  trash node_modules &>/dev/null;
  # should almost always be angular
  preset=${2:-$(conventional-commits-detector)}
  bump=${1:-$(conventional-recommended-bump -p $preset)}
  if [ -z $CONVENTIONAL_GITHUB_RELEASER_TOKEN ]; then
    echo "CONVENTIONAL_GITHUB_RELEASER_TOKEN not found"
    return 1;
  fi
  echo "Bump: $bump."
  echo "Preset: $preset"
  echo "Do you wish to continue?"
  select yn in "Yes" "No"; do
    case $yn in
      Yes ) break;;
      No ) return 0;;
    esac
  done
  # make sure we have the latest commits
  git pull --rebase &&
    # get fresh packages
    npm ci &&
    # make sure tests pass
    npm test &&
    # copy package.json for later restoration
    cp package.json _package.json &&
    # copy package-lock if exists
    [[ -e package-lock.json ]] && cp package-lock.json _package-lock.json || echo "info: no package-lock found" &&
    npm --no-git-tag-version version $bump &>/dev/null &&
    echo "creating/updating changelong" &&
    conventional-changelog -i CHANGELOG.md -s -p $preset &&
    git add CHANGELOG.md &&
    version=`cat package.json | json version` &&
    echo "Adding changelog" &&
    git commit -m "docs(CHANGELOG): v$version" &&
    mv -f _package.json package.json &&
    [[ -e _package-lock.json ]] && mv -f _package-lock.json package-lock.json ||
    echo "cutting release" &&
    npm version $bump -m "chore(release): v%s" &&
    echo "pushing up changes" &&
    git push --follow-tags &&
    echo "creating github release" &&
    conventional-github-releaser -p $preset &&
    echo "publishing to npm" &&
    npm publish
}
