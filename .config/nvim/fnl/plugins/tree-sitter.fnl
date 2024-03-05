(module plugins.tree-sitter
  {autoload
   {a aniseed.core
    r r
    ts nvim-treesitter
    tsconfigs nvim-treesitter.configs
    tshighlights nvim-treesitter.highlight
    queries nvim-treesitter.query
    parsers nvim-treesitter.parsers
    nvim-ts-install nvim-treesitter.install
    vim-ts-queries vim.treesitter.query}
   require-macros [macros]})

(def- ensure_modules
  [:nvim-treesitter
   :nvim-treesitter-refactor
   :playground
   :nvim-treesitter-context])

(def- rainbow
  ["#8BE9FD"
   "#50FA7B"
   "#FFB86C"
   "#FF79C6"
   "#BD93F9"
   "#FF5555"
   "#F1FA8C"])

(defn main []
  (tsconfigs.setup
    {:ensure_installed
     [:bash
      :c ; required by nvim-treesitter or help docs are broken??
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
      :prisma
      :pug
      :query
      :regex
      :rust
      :solidity
      :swift
      :templ
      :toml
      :tsx
      :typescript
      :vim
      :vimdoc] ; required by nvim-treesitter or help docs are broken

     :highlight {:enable true}
     :incremental_selection {:enable true
                             :keymaps                    ; mappings for incremental selection (visual mappings)
                             {:init_selection "gni"      ; maps in normal mode to init the node/scope selection
                              :node_incremental "gni"    ; increment to the upper named parent
                              :scope_incremental "gci"   ; increment to the upper scope (as defined in locals.scm)
                              :node_decremental "gnd"}}  ; decrement to the previous node

     :indent {:enable true
              :disable [:fennel]}

     :playground {:enable true}
     :query_linter {:enable true}
     :matchup {:enable true :disable [:c]}}

    (-?> _G
      (a.get-in [:vim :treesitter :highlighter :hl_map])
      (r.assoc :error nil)
      (r.assoc :punctuation.delimiter "Delimiter")
      (r.assoc :punctuation.bracket nil))))
