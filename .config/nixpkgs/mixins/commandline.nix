{ pkgs, ... }:
{
  home.packages = with pkgs; [
    antigen # zsh plugin manager
    direnv # A shell extension that manages your environment
    handlr # Alternative to xdg-open to manage default applications with ease
    hpack # A command-line tool for creating and manipulating Haskell packages
    htop # An interactive process viewer
    dogdns # A command-line DNS cliento
    du-dust # A more intuitive version of du in rust
    exa # A modern replacement for ls
    fd # A simple, fast and user-friendly alternative to find
    fzf # A command-line fuzzy finder
    glow # Render markdown on the CLI, with pizzazz!
    lazygit # simple terminal UI for git commands
    rsync # Fast incremental file transfer utility
    rustscan # The Modern Port Scanner
    silver-searcher # A code-searching tool similar to ack, but faster
    taskwarrior # A command-line todo list manager
    timewarrior # A command-line time tracker
    xplugd # A UNIX daemon that executes a script on X input and RandR changes
    vimv # batch rename files w/ vim
    p7zip # 7-Zip is a file archiver with a high compression ratio
  ];

  programs = {
    # A cat(1) clone with wings
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [
        batman
        batpipe
      ];
    };
  };

}
