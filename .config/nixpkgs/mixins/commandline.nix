{ pkgs, ... }:
{
  home.packages = with pkgs; [
    antigen # zsh plugin manager
    bind # dns client
    curl # transfer data from or to a server
    direnv # A shell extension that manages your environment
    dogdns # A command-line DNS cliento
    du-dust # A more intuitive version of du in rust
    exa # A modern replacement for ls
    fd # A simple, fast and user-friendly alternative to find
    fzf # A command-line fuzzy finder
    gfold # CLI tool to help keep track of your Git repositories, written in Rust
    gh # GitHub CLI
    glow # Render markdown on the CLI, with pizzazz!
    handlr # Alternative to xdg-open to manage default applications with ease
    htop # An interactive process viewer
    hub # A command-line tool that makes git easier to use with GitHub
    iputils # Network monitoring tools including ping
    lazygit # simple terminal UI for git commands
    lsof # lists open files
    # neovim # Better vim # need to fix
    p7zip # 7-Zip is a file archiver with a high compression ratio
    playerctl # pause/play music players ci
    procs # A modern replacement for ps written in Rust
    ripgrep # recursively searches directories for a regex pattern
    rsync # Fast incremental file transfer utility
    rustscan # The Modern Port Scanner
    silver-searcher # A code-searching tool similar to ack, but faster
    taskwarrior # A command-line todo list manager
    tealdeer # A very fast implementation of tldr in Rust
    timewarrior # A command-line time tracker
    tmux # A terminal multiplexer
    traceroute # print the route packets trace to network host
    vimv # batch rename files w/ vim
    wakatime # Command line interface for Wakatime
    wget # The non-interactive network downloader # required by jdownloader
    yadm # Yet Another Dotfiles Manager
    zoxide # A fast alternative to cd that learns your habits
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
