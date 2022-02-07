(module plugins.conjure
  {require {: utils}
   require-macros [macros]})


(defn main []
  (utils.set-nvim-g!
    {:conjure#client#fennel#aniseed#aniseed_module_prefix "aniseed."}))
