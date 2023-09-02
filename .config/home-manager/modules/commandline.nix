{ pkgs, lib, ... }:
let
  getOptions = pkgs.getoptions.overrideAttrs (old: {
    doCheck = false; # Will break on yash otherwise
  });
  dotDir = ".config/";
  relToDotDir = file: dotDir + file;

  projects = pkgs.writeShellScriptBin "projects" ''
    # This script is used to find all the top level git folders in a directory
    _get_git_folders() {
      local dir="$1"
      local git_folder

      # throw error if no directory is passed
      if [ -z "$dir" ]; then
        echo "No directory passed"
        exit 1
      fi

      while read -r git_folder; do
        echo "$git_folder"
      done < <(fd ".git$" "$dir" -t d --hidden | xargs dirname | xargs realpath | sort)
    }

    _get_git_folders "$@"
  '';
in
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
    getOptions # A library for parsing command line options
    gh # GitHub CLI
    glow # Render markdown on the CLI, with pizzazz!
    handlr # Alternative to xdg-open to manage default applications with ease
    htop # An interactive process viewer
    hub # A command-line tool that makes git easier to use with GitHub
    iputils # Network monitoring tools including ping
    lazygit # simple terminal UI for git commands
    lsof # lists open files
    p7zip # 7-Zip is a file archiver with a high compression ratio
    playerctl # pause/play music players ci
    procs # A modern replacement for ps written in Rust
    projects # A script to find all the top level git folders in a directory
    ripgrep # recursively searches directories for a regex pattern
    rsync # Fast incremental file transfer utility
    rustscan # The Modern Port Scanner
    silver-searcher # A code-searching tool similar to ack, but faster
    shfmt # A shell parser, formatter, and interpreter (POSIX/Bash/mksh)
    taskwarrior # A command-line todo list manager
    tealdeer # A very fast implementation of tldr in Rust
    timewarrior # A command-line time tracker
    traceroute # print the route packets trace to network host
    vimv # batch rename files w/ vim
    wakatime # Command line interface for Wakatime
    wget # The non-interactive network downloader # required by jdownloader
    wordnet # Lexical database for the English language
    yadm # Yet Another Dotfiles Manager
    yt-dlp # A youtube-dl fork with additional features and fixes
    zoxide # A fast alternative to cd that learns your habits
    zsh # A shell designed for interactive use, although it is also a powerful scripting language
  ];

  home.file."${relToDotDir "taskell/config.ini"}".text = lib.generators.toINI { } {
    general = {
      filename = "kanban.md";
    };

    layout = {
      padding = 1;
      column_width = 30;
      column_padding = 3;
      description_indicator = "≡";
      statusbar = true;
    };

    markdown = {
      title = "##";
      task = "-";
      summary = "    >";
      due = "    @";
      subtask = "    *";
      localTimes = false;
    };
  };

  home.file."${relToDotDir "zsh/nix-packages.zsh"}".source = pkgs.writeText "nix-packages" ''
    source "${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh"
  '';

  programs = {
    # A cat(1) clone with wings
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [
        batman
        batpipe
      ];
    };

    # Vim text editor fork focused on extensibility and agility
    neovim = {
      enable = true;
      extraPackages = with pkgs; [
        parinfer-rust
        tree-sitter # to build grammars from source
        lua-language-server
      ];

      plugins = with pkgs.vimPlugins; [
        aniseed
        lazy-nvim
        parinfer-rust
      ];

      extraLuaPackages = luaPkgs: with luaPkgs; [
        jsregexp # for luasnip
      ];
    };
  };
}
