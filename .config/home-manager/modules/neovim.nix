{self, ...}: {
  flake.modules.homeManager.neovim = {pkgs, ...}: {
    imports = [
      self.modules.homeManager.parinfer
    ];

    programs.neovim = {
      enable = true;
      extraLuaPackages = ps:
        with ps; [
          jsregexp # for luasnip
          toml-edit # toml parser
          lyaml # yaml parser
        ];
      extraPackages = with pkgs; [
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

        prettier # for formatting js, ts, css, html, json, yaml, markdown
        sql-formatter # formatting sql
        vim-language-server # Language server for Vim script

        yaml-language-server # A language server for YAML
        bash-language-server # A language server for Bash
        typescript-language-server # TypeScript & JavaScript Language Server
        vscode-langservers-extracted # Language servers extracted from VSCode, css mainly
        tailwindcss-language-server # Language server for Tailwind CSS
      ];

      plugins = with pkgs.vimPlugins; [
        parinfer-rust
        aniseed
        lazy-nvim
      ];
      sideloadInitLua = true;
      withPython3 = false;
      withRuby = false;
      withNodeJs = false;
    };
  };
}
