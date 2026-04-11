(module plugins.treesitter
  {autoload
   {a aniseed.core
    r r
    ts nvim-treesitter}
   require-macros [macros]})

(defn main []
  (ts.install
    [:bash
     :c ; required by nvim or help docs are broken
     :clojure
     :css
     :dockerfile
     :fennel
     :go
     :haskell
     :html
     :javascript
     :jsdoc
     :json
     :lua
     :markdown
     :markdown_inline
     :nix
     :prisma
     :pug
     :query
     :regex
     :rust
     :solidity
     :slint
     :swift
     :templ
     :toml
     :tsx
     :typescript
     :vim
     :vimdoc]) ; required by nvim or help docs are broken

  ; enable treesitter highlighting for all filetypes
  (vim.api.nvim_create_autocmd :FileType
    {:pattern :*
     :callback #(pcall vim.treesitter.start)})

  ; treesitter-based folding
  (vim.api.nvim_create_autocmd :FileType
    {:pattern :*
     :callback (fn []
                 (tset vim.wo 0 0 :foldmethod :expr)
                 (tset vim.wo 0 0 :foldexpr "v:lua.vim.treesitter.foldexpr()"))}))
