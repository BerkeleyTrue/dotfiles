{pkgs, ...}: {
  home.packages = with pkgs; [
    mongodb-compass # mongodb UI tool -- non-free
    nodePackages.json # A JSON parser and stringifier for JavaScript
    nodePackages.bash-language-server # A language server for Bash
    nil #nix language server
    nixd # nix language server
    stack # The Haskell Tool Stack
  ];
}
