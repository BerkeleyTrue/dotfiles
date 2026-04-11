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

  (augroup :TreeSitterPluggin
    ; enable treesitter highlighting for all filetypes
    {:event [:BufReadPost :BufNewFile]
     :pattern :*
     :callback (fn [{: buf}]
                 (let [ft (bo filetype)
                       bt (bo buftype)]
                   (when (= bt "")
                     (bo! indentexpr "v:lua.require('nvim-treesitter').indentexpr()")
                     (when (and (vim.treesitter.language.get_lang ft)
                                (r.contains? (ts.get_installed) ft))
                       (pcall vim.treesitter.start))))
                 nil)}))

    ; treesitter-based folding
    ; {:event :FileType
    ;  :pattern :*
    ;  :callback (fn []
    ;              (tset vim.wo 0 0 :foldmethod :expr)
    ;              (tset vim.wo 0 0 :foldexpr "v:lua.vim.treesitter.foldexpr()"))}))
