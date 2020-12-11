(module plugins.tree-sitter
  {:require {a aniseed.core
             : utils
             : r}})


(def- ts [:nvim-treesitter
          :nvim-treesitter-refactor
          :playground
          :nvim-treesitter-context])

(defn main []
  (let [ok (->>
             ts
             (r.reduce
               #(let [ok (pcall utils.ex.packadd $2)]
                 (if
                   ; if ok and past val is true, send true, otherwise send false
                   ok (and $1 true)
                   (print (.. "Could not load " $2))))

               true))]


    (when ok
      (let [tsconfigs (require :nvim-treesitter.configs)
            tshighlights (require :nvim-treesitter.highlight)]


        (do
          (tsconfigs.setup
            {
             :ensure_installed "maintained"
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
             :indent {:enable true}

             :refactor
             {
              :highlight_definitions {:enable true}
              :highlight_current_scope {:enable false}
              :smart_rename
              {
               :enable true
               :keymaps {:smart_rename "grr"}}}

             :playground {:enable true}})

          (-?> _G
            (a.get-in [:vim :treesitter :highlighter :hl_map])
            (r.assoc :error nil)
            (r.assoc :punctuation.delimiter "Delimiter")
            (r.assoc :punctuation.bracket nil)))))))
