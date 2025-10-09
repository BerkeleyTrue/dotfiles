(module plugins.markdown
  {require {utils utils}})


(defn main []
  (utils.set-nvim-g!
    {:markdown_enable_mappings false}))
