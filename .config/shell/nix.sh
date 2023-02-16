nin() {
  package=${1-}
  if [ -z "$package" ]; then
	echo "no package supplied"
	return 1
  fi
  nix-env -iA nixpkgs.$package
}
