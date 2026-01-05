{
  pkgs,
  lib,
  ...
}: let
  getOptions = pkgs.getoptions.overrideAttrs (old: {
    doCheck = false; # Will break on yash otherwise
  });

  projects = pkgs.writeShellApplication {
    name = "projects";
    runtimeInputs = [
      pkgs.fd
    ];
    text = ''
      # shellcheck disable=SC2016
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
        done < <(fd ".git$" "$dir" --type dir --hidden | xargs dirname | xargs realpath | sort)

        while read -r wt_folder; do
          echo "$wt_folder"
        done < <(fd ".bare$" "$dir" --type dir --hidden |\
                  xargs -I {} sh -c 'find "$(dirname {})" -maxdepth 1 -type d ! -name ".*" ! -path "$(dirname {})"' |\
                  xargs realpath | sort)
      }

      _get_git_folders "$@"
    '';
  };
in {
  catppuccin.eza.enable = true;
  home.packages =
    (with pkgs; [
      age # A simple, modern and secure encryption tool with small explicit keys, no config options, and UNIX-style composability
      antigen # zsh plugin manager
      babashka # A Clojure babushka for the grey areas of bash
      bind # dns client
      bitwarden-cli # a secure and free password manager for all of your devices
      bluetuith # bluetooth manager tui
      bottom # A cross-platform graphical process/system monitor with a customizable interface and a multitude of features
      claude-code # A command line interface for Claude AI
      codex # A lightweight coding agent that runs in your terminal
      curl # transfer data from or to a server
      crush # The glamourous AI coding agent for your favourite terminal
      delta # A syntax-highlighter for git and diff output
      dogdns # A command-line DNS cliento
      dust # A more intuitive version of du in rust
      eza # A modern replacement for ls
      entr # Run arbitrary commands when files change
      fd # A simple, fast and user-friendly alternative to find
      fzf # A command-line fuzzy finder
      gh # GitHub CLI
      glow # Render markdown on the CLI, with pizzazz!
      handlr # Alternative to xdg-open to manage default applications with ease
      htop # An interactive process viewer
      instaloader # Download public and private instagram accounts
      iputils # Network monitoring tools including ping
      just # A command runner with a makefile-like syntax
      jq # Command-line JSON processor
      lazydocker # simple terminal ui for both docker and docker-compose
      lsof # lists open files
      mermaid-cli # Generation of diagrams and flowcharts from text in a similar manner as markdown
      ngrok # Introspected tunnels to localhost
      p7zip # 7-Zip is a file archiver with a high compression ratio
      playerctl # pause/play music players ci
      podman # A program for managing pods, containers and container images
      podman-tui # Podman Terminal UI
      presenterm # A terminal presentation tool
      procs # A modern replacement for ps written in Rust
      ripgrep-all # ripgrep but also search in Pdfs, zip, and more
      ripgrep # recursively searches directories for a regex pattern
      rsync # Fast incremental file transfer utility
      rustscan # The Modern Port Scanner
      shfmt # A shell parser, formatter, and interpreter (POSIX/Bash/mksh)
      silver-searcher # A code-searching tool similar to ack, but faster
      smartmontools # control and monitor storage systems using S.M.A.R.T.
      sops # Simple and flexible tool for managing secrets
      ssh-to-age # convert ssh private keys in ed25519 format to age keys
      taskwarrior3 # A command-line todo list manager
      tealdeer # A very fast implementation of tldr in Rust
      timewarrior # A command-line time tracker
      traceroute # print the route packets trace to network host
      udiskie # Removable disk automounter using udisks
      udisks # access and manipulate disks and media devices
      vimv # batch rename files w/ vim
      wakatime-cli # Command line interface for Wakatime
      wget # The non-interactive network downloader # required by jdownloader
      wiki-tui # A terminal based wikipedia viewer
      wordnet # Lexical database for the English language
      xdotool # Command-line X11 automation tool
      dragon-drop # simple drag and drop source/sink for x11
      xh # Friendly and fast tool for sending HTTP requests,  curl/wget
      yay # yet another aur helper
      yadm # Yet Another Dotfiles Manager
      yt-dlp # A youtube-dl fork with additional features and fixes
      yubikey-manager # A command line tool to manage YubiKeys
      zellij # A terminal workspace and multiplexer
      zoxide # A fast alternative to cd that learns your habits
      zsh # A shell designed for interactive use, although it is also a powerful scripting language
    ])
    ++ [
      getOptions # A library for parsing command line options
      projects # A script to find all the top level git folders in a directory
    ];

  xdg.configFile."taskell/config.ini".text = lib.generators.toINI {} {
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

  xdg.configFile."zsh/nix-packages.zsh".source = pkgs.writeText "nix-packages" ''
    source "${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh"
  '';

  xdg.configFile."containers/registries.conf".text = ''
    unqualified-search-registries = ["docker.io"]

    [[registry]]
    prefix = "docker.io"
    insecure = false
    blocked = false
    location = "docker.io"
  '';

  # required for copilot.lua to work.
  xdg.configFile."nvim/lua/copilot-nodejs.lua".source = pkgs.writeText "copilot-nodejs" ''
    vim.g.copilot_node_command = "${pkgs.nodejs_24}/bin/node"
  '';

  catppuccin.bat.enable = true;
  catppuccin.lazygit.enable = true;
  programs = {
    # A cat(1) clone with wings
    bat = {
      enable = true;

      config.map-syntax = [
        "*.fnl:Clojure"
        "*.templ:Go"
        "flake.lock:JSON"
      ];

      extraPackages = with pkgs.bat-extras; [
        batman
        batpipe
      ];
    };

    # Vim text editor fork focused on extensibility and agility
    neovim = {
      enable = true;
      extraPackages = with pkgs;
        [
          parinfer-rust
          tree-sitter # to build grammars from source
          nodejs_24

          lua-language-server # A language server for Lua
          nil #nix language server
          nixd # nix language server
          clojure-lsp
          slint-lsp
          rust-analyzer # rust language server
          dockerfile-language-server # docker language server
          basedpyright # python language server

          alejandra # formatting nix
          shfmt # formatting shell scripts
          rustfmt # formatting rust

          golines # formatting long lines in go
          templ # formatting go templ files
          gopls # Go language server
        ]
        ++ (with nodePackages; [
          yaml-language-server # A language server for YAML
          bash-language-server # A language server for Bash
          typescript-language-server # TypeScript & JavaScript Language Server
          vscode-langservers-extracted # Language servers extracted from VSCode, css mainly
          tailwindcss-language-server # Language server for Tailwind CSS
          vim-language-server # Language server for Vim script

          prettier # for formatting js, ts, css, html, json, yaml, markdown
          # purs-tidy # formatting purescript (not in nixpkgs anymore)
          sql-formatter # formatting sql
        ]);

      plugins = with pkgs.vimPlugins; [
        parinfer-rust
        aniseed
        lazy-nvim
      ];

      extraLuaPackages = luaPkgs:
        with luaPkgs; [
          jsregexp # for luasnip
          toml-edit # toml parser
          lyaml # yaml parser
        ];
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    lf = {
      enable = false;
      commands = {
        dragon-out = ''%${pkgs.xdragon}/bin/xdragon -a -x "$fx"'';
        editor-open = ''$$EDITOR $f'';
        mkdir = ''$mkdir -p "$(echo $* | tr ' ' '\ ')"'';
        unzip = ''$7z -x "$f"'';
        unzip-to = ''$7z -x -o"$(echo $* | tr ' ' '\ ')" "$f"'';
      };

      keybindings = {
        "\\\"" = "";
        o = "";
        "." = "set hidden!";
        "`" = "mark-load";
        "\\'" = "mark-load";

        do = "dragon-out";
        D = "delete";

        g = "top";
        G = "bottom";

        "g~" = "cd"; # go to home directory
        gh = "cd"; # go to home directory
        "g/" = "/"; # go to root directory

        A = ":rename; cmd-end"; # at the very end
        c = "push A<c-u>"; # new rename
        I = ":rename; cmd-home"; # at the very beginning
        i = ":rename"; # before extension
        a = ":rename; cmd-right"; # after extension

        V = ''push :!nvim<space>'';

        # ...
      };

      settings = {
        preview = true;
        hidden = true;
        drawbox = true;
        icons = true;
        ignorecase = true;
      };

      previewer = {
        keybinding = "i";
        source = "${pkgs.ctpv}/bin/ctpv";
      };

      extraConfig = ''
        &${pkgs.ctpv}/bin/ctpv -s $id

        cmd on-quit %${pkgs.ctpv}/bin/ctpv -e $id
        set cleaner ${pkgs.ctpv}/bin/ctpvclear
      '';
    };

    atuin = {
      enable = true;
      settings = {
        enter_accept = false;
        keymap_mode = "vim-normal";
        inline_height = 20;
      };
    };

    # Blazingly fast terminal file manager
    yazi = {
      enable = true;
      settings = {
        mgr = {
          show_hidden = true;
        };
        opener.open = [
          {
            run = ''xdg-open "$1"'';
            desc = "Open";
            orphan = true;
          }
        ];
      };
    };

    lazygit = {
      enable = true;
      settings = {
        customCommands = [
          {
            key = "w";
            context = "files";
            command = "git commit --no-verify";
            description = "Commit without verifying";
            output = "terminal";
          }
        ];
        gui.theme = {
          selectedLineBgColor = ["default"];
        };
      };
    };
  };
}
