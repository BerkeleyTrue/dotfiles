{ config, pkgs, user, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = "/home/${user}";

  home.packages = with pkgs; [
    antigen # zsh plugin manager
    direnv # A shell extension that manages your environment
    # getoptions # better options parser for posix -- broken?
    haskellPackages.status-notifier-item # sni system tray protocol
    hpack # A command-line tool for creating and manipulating Haskell packages
    htop # An interactive process viewer
    mongodb-compass  # mongodb UI tool -- non-free
    postman # API Development Environment
    nil #nix language server
    stack # The Haskell Tool Stack
    vimv # batch rename files w/ vim
    vlc # Cross-platform media player and streaming server
    xplugd # A UNIX daemon that executes a script on X input and RandR changes
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
