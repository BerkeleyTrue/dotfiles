(module plugins.beacon
  {:require {nvim aniseed.nvim}})

(defn main []
  (tset nvim.g :beacon_ignore_buffers ["\\w*fugitive*\\w"]))
