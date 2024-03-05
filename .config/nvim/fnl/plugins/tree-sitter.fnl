(module plugins.tree-sitter
  {autoload
   {a aniseed.core
    r r
    tsconfigs nvim-treesitter.configs}
   require-macros [macros]})

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
     :matchup {:enable true :disable [:c]}}))
