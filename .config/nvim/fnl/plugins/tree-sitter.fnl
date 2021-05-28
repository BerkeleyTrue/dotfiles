(module plugins.tree-sitter
  {:require {: r
             : utils
             a aniseed.core}})


(def- ts [:nvim-treesitter
          :nvim-treesitter-refactor
          :playground
          :nvim-treesitter-context])

(defn get-ft-query [ft type]
  (let [path (.. (vim.fn.stdpath :config) (.. "/queries/" ft "/" type ".scm"))]
    (vim.fn.join (vim.fn.readfile path) "\n")))

(defn main []
  (let [ok (->>
             ts
             (r.reduce
               #(let [(ok res) (pcall utils.ex.packadd $2)]
                 (if
                   ; if ok and past val is true, send true, otherwise send false
                   ok (and $1 true)
                   (print (.. "Could not load " $2 ": " res))))
               true))]
    (when ok
      (let [ts (require :nvim-treesitter)
            tsconfigs (require :nvim-treesitter.configs)
            tshighlights (require :nvim-treesitter.highlight)
            queries (require :nvim-treesitter.query)
            parsers (require :nvim-treesitter.parsers)
            nvim-ts-install (require :nvim-treesitter.install)
            parser-conf (parsers.get_parser_configs)
            vim-ts-queries (require :vim.treesitter.query)]

        (ts.define_modules
          {:cindent
           {:module_path :ts.indents
            :is_supported
            (fn [lang]
              (not (= (queries.get_query lang "indents") nil)))}})

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
            :fennel
            :html
            :javascript
            :jsdoc
            :json
            :lua
            :query
            :regex
            :rust
            :solidity
            :tsx
            :typescript]

           :highlight {:enable true}
           :incremental_selection
           {
            :enable true
            :keymaps
            {                           ; mappings for incremental selection (visual mappings)
             :init_selection "gni"      ; maps in normal mode to init the node/scope selection
             :node_incremental "gni"    ; increment to the upper named parent
             :scope_incremental "gci"   ; increment to the upper scope (as defined in locals.scm)
             :node_decremental "gnd"}}  ; decrement to the previous node}}
           :cindent {:enable false}
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
           :query_linter {:enable true}})

        ; don't set queries unless fennel parser is already present
        ; otherwise nvim-ts will error error out and prevent the setup
        ; will not work on the same pass as fennel parser install so a restart is necessary
        (when (parsers.has_parser :fennel)
          (vim-ts-queries.set_query
            :fennel
            :highlights
            (get-ft-query :fennel :highlights))

          (vim-ts-queries.set_query
            :fennel
            :locals
            (get-ft-query :fennel :locals)))

        (-?> _G
          (a.get-in [:vim :treesitter :highlighter :hl_map])
          (r.assoc :error nil)
          (r.assoc :punctuation.delimiter "Delimiter")
          (r.assoc :punctuation.bracket nil))))))
