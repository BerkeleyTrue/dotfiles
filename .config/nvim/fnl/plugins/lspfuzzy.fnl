(module plugins.lspfuzzy
  {require {: r
            : utils}
   require-macros [macros]})

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-lspfuzzy)]
    (if (not ok) (print (.. "Could not load nvim-lspfuzzy: " (tostring res)))
      (let [(ok lspfuzzy) (pcall require :lspfuzzy)]
        (if (not ok) (print (.. "require: " lspfuzzy))
          (lspfuzzy.setup {}))))))
