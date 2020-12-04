(module plugins.startify
  {:require {utils utils}})

(defn main []
  (utils.set-nvim-g! {:startify_change_to_dir 0}))
