(module plugins.markdown
  {:require {utils utils}})


(defn main []
  (utils.set-nvim-g!
    {:markdown_enable_insert_mode_mappings true}))
