{
  pkgs,
  lib,
  ...
}: let
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

  bat-catppucchin = pkgs.fetchFromGitHub {
    owner = "catppuccin";
    repo = "bat";
    rev = "ba4d16880d63e656acced2b7d4e034e4a93f74b1";
    sha256 = "sha256-6WVKQErGdaqb++oaXnY3i6/GuH2FhTgK0v4TN4Y0Wbw=";
  };
in {
  home.packages = with pkgs; [
    antigen # zsh plugin manager
    bind # dns client
    curl # transfer data from or to a server
    dogdns # A command-line DNS cliento
    du-dust # A more intuitive version of du in rust
    eza # A modern replacement for ls
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
    ngrok # Introspected tunnels to localhost
    p7zip # 7-Zip is a file archiver with a high compression ratio
    playerctl # pause/play music players ci
    procs # A modern replacement for ps written in Rust
    projects # A script to find all the top level git folders in a directory
    ripgrep # recursively searches directories for a regex pattern
    rsync # Fast incremental file transfer utility
    rustscan # The Modern Port Scanner
    shfmt # A shell parser, formatter, and interpreter (POSIX/Bash/mksh)
    silver-searcher # A code-searching tool similar to ack, but faster
    taskwarrior # A command-line todo list manager
    tealdeer # A very fast implementation of tldr in Rust
    timewarrior # A command-line time tracker
    traceroute # print the route packets trace to network host
    # udisks # access and manipulate disks and media devices
    # udiskie # Removable disk automounter using udisks
    vimv # batch rename files w/ vim
    wakatime # Command line interface for Wakatime
    wget # The non-interactive network downloader # required by jdownloader
    wordnet # Lexical database for the English language
    yadm # Yet Another Dotfiles Manager
    yt-dlp # A youtube-dl fork with additional features and fixes
    zoxide # A fast alternative to cd that learns your habits
    zsh # A shell designed for interactive use, although it is also a powerful scripting language
  ];

  home.file."${relToDotDir "taskell/config.ini"}".text = lib.generators.toINI {} {
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

      config = {
        theme = "catppuccin";
        map-syntax = [
          "*.fnl:Clojure"
          "*.templ:Go"
          "flake.lock:JSON"
        ];
      };

      themes = {
        catppuccin = {
          src = bat-catppucchin;
          file = "/Catppuccin-frappe.tmTheme";
        };
      };

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

        lua-language-server # A language server for Lua
        nodePackages.bash-language-server # A language server for Bash
        nil #nix language server
        nixd # nix language server

        nodePackages.prettier # for formatting js, ts, css, html, json, yaml, markdown
        alejandra # formatting nix
        shfmt # formatting shell scripts
        nodePackages.purs-tidy # formatting purescript
        golines # formatting long lines in go
      ];

      plugins = with pkgs.vimPlugins; [
        parinfer-rust
        aniseed
        lazy-nvim
      ];

      extraLuaPackages = luaPkgs:
        with luaPkgs; [
          jsregexp # for luasnip
        ];
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    lf = {
      enable = true;
      commands = {
        dragon-out = ''%${pkgs.xdragon}/bin/xdragon -a -x "$fx"'';
        editor-open = ''$$EDITOR $f'';
        mkdir = ''
          ''${{
            printf "Directory Name: "
            read DIR
            mkdir -p $DIR
          }}
        '';
      };

      keybindings = {
        "\\\"" = "";
        o = "";
        c = "mkdir";
        "." = "set hidden!";
        "`" = "mark-load";
        "\\'" = "mark-load";
        "<enter>" = "open";

        do = "dragon-out";

        "g~" = "cd";
        gh = "cd";
        "g/" = "/";

        ee = "editor-open";
        V = ''$${pkgs.bat}/bin/bat --paging=always "$f"'';

        # ...
      };

      settings = {
        preview = true;
        hidden = true;
        drawbox = true;
        icons = true;
        ignorecase = true;
      };

      extraConfig = let
        previewer = pkgs.writeShellScriptBin "pv.sh" ''
          file=$1
          w=$2
          h=$3
          x=$4
          y=$5

          if [[ "$( ${pkgs.file}/bin/file -Lb --mime-type "$file")" =~ ^image ]]; then
              ${pkgs.kitty}/bin/kitty +kitten icat --silent --stdin no --transfer-mode file --place "''${w}x''${h}@''${x}x''${y}" "$file" < /dev/null > /dev/tty
              exit 1
          fi

          ${pkgs.pistol}/bin/pistol "$file"
        '';

        cleaner = pkgs.writeShellScriptBin "clean.sh" ''
          ${pkgs.kitty}/bin/kitty +kitten icat --clear --stdin no --silent --transfer-mode file < /dev/null > /dev/tty
        '';
      in ''
        set cleaner ${cleaner}/bin/clean.sh
        set previewer ${previewer}/bin/pv.sh
      '';
    };
  };
}
