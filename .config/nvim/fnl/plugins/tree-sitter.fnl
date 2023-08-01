(module plugins.tree-sitter
  {require
   {: r
    : utils
    md utils.module
    a aniseed.core}
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

(defn- get-ft-query [ft type]
  (let [path (.. (vim.fn.stdpath :config) (.. "/queries/" ft "/" type ".scm"))]
    (vim.fn.join (vim.fn.readfile path) "\n")))

(defn main []
  (when-let [ts (md.prequire :nvim-treesitter)]
    (let [tsconfigs (md.prequire :nvim-treesitter.configs)
          tshighlights (md.prequire :nvim-treesitter.highlight)
          queries (md.prequire :nvim-treesitter.query)
          parsers (md.prequire :nvim-treesitter.parsers)
          nvim-ts-install (md.prequire :nvim-treesitter.install)
          parser-conf (parsers.get_parser_configs)
          vim-ts-queries (md.prequire :vim.treesitter.query)]

      (tset
        parser-conf
        :fennel
        {:install_info
         {:url "~/dvlpmnt/lisp/fennel/tree-sitter-fennel"
          :files [:src/parser.c]
          :requires_generate_from_grammar true}
         :filetype :fennel})

      (tset
        parser-conf
        :solidity
        {:install_info
         {:url "https://github.com/JoranHonig/tree-sitter-solidity"
          :files [:src/parser.c]
          :requires_generate_from_grammar true}
         :filetype :solidity})

      (tsconfigs.setup
        {:ensure_installed
         [:bash
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
          :prisma
          :pug
          :query
          :regex
          :rust
          :solidity
          :swift
          :tsx
          :typescript
          :vim]

         :highlight {:enable true}
         :incremental_selection
         {
          :enable true
          :keymaps
          {                           ; mappings for incremental selection (visual mappings)
           :init_selection "gni"      ; maps in normal mode to init the node/scope selection
           :node_incremental "gni"    ; increment to the upper named parent
           :scope_incremental "gci"   ; increment to the upper scope (as defined in locals.scm)
           :node_decremental "gnd"}}  ; decrement to the previous node
         :indent {:enable true}

         :refactor
         {
          :highlight_definitions {:enable true}
          :highlight_current_scope {:enable false}
          :smart_rename
          {
           :enable true
           :keymaps {:smart_rename "grr"}}}

         :playground {:enable true}
         :query_linter {:enable true}
         :rainbow
         {:enable true
          :colors rainbow}
         :matchup {:enable true}
         :context_commentstring {:enable true}})


      ; don't set queries unless fennel parser is already present
      ; otherwise nvim-ts will error out and prevent the setup
      ; will not work on the same pass as fennel parser install so a restart is necessary
      (when (parsers.has_parser :fennel)
        (vim-ts-queries.set
          :fennel
          :highlights
          (get-ft-query :fennel :highlights))

        (vim-ts-queries.set
          :fennel
          :locals
          (get-ft-query :fennel :locals)))

      (-?> _G
        (a.get-in [:vim :treesitter :highlighter :hl_map])
        (r.assoc :error nil)
        (r.assoc :punctuation.delimiter "Delimiter")
        (r.assoc :punctuation.bracket nil)))))
