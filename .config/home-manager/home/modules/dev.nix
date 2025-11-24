{pkgs, ...}: {
  home.packages = with pkgs; [
    mongodb-compass # mongodb UI tool -- non-free
    # nodePackages.json # A JSON parser and stringifier for JavaScript # no longer maintained
    stack # The Haskell Tool Stack
  ];
}
