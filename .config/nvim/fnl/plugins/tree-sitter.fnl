(module plugin.tree-siter
  {:require {a aniseed.core
             nvim aniseed.nvim
             tsconfigs nvim-treesitter.configs
             tshighlights nvim-treesitter.highlight}})


(tsconfigs.setup {:ensure_installed "maintained"
                  :highlight {:enable true}
                  :indent {:enable true}
                  :refactor {:highlight_definitions {:enable true}
                             :highlight_current_scope {:enable false}}
                  :playground {:enable true}})

(-?> _G
  (a.get-in [:vim :treesitter :highlighter :hl_map])
  (a.assoc :error nil)
  (a.assoc :punctuation.delimiter "Delimiter")
  (a.assoc :punctuation.bracket nil))
