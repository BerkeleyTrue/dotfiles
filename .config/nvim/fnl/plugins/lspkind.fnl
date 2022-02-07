(module plugins.lspkind
  {require {: utils}})


(defn main []
  (let [(ok res) (pcall utils.ex.packadd :lspkind-nvim)]
    (if (not ok) (print (.. "Could not load lspkind: " (tostring res)))
      (let [(ok lspkind) (pcall require :lspkind)]
        (if (not ok) (print (.. "require: " lspkind))
          (lspkind.init))))))
