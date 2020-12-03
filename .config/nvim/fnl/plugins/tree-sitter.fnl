(module plugins.tree-sitter
  {:require {a aniseed.core
             nvim aniseed.nvim
             tsconfigs nvim-treesitter.configs
             tshighlights nvim-treesitter.highlight}})


(defn main []
  (do
    (tsconfigs.setup {:ensure_installed "maintained"
                      :highlight {:enable true}
                      :incremental_selection {:enable true
                                              :keymaps {                           ; mappings for incremental selection (visual mappings)
                                                        :init_selection "gni"      ; maps in normal mode to init the node/scope selection
                                                        :node_incremental "gni"    ; increment to the upper named parent
                                                        :scope_incremental "gci"   ; increment to the upper scope (as defined in locals.scm)
                                                        :node_decremental "gnd"}}  ; decrement to the previous node}}
                      :indent {:enable true}

                      :refactor {:highlight_definitions {:enable true}
                                 :highlight_current_scope {:enable false}
                                 :smart_rename {:enable true
                                                :keymaps {:smart_rename "grr"}}}


                      :playground {:enable true}})

    (-?> _G
      (a.get-in [:vim :treesitter :highlighter :hl_map])
      (a.assoc :error nil)
      (a.assoc :punctuation.delimiter "Delimiter")
      (a.assoc :punctuation.bracket nil))))
