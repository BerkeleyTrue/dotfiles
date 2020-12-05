(module plugins.easy-align
  {:require {utils utils}})

(defn main []
  (utils.pack-add :vim-easy-align)
  (utils.ex.xmap  [:ga "<Plug>(EasyAlign)"])
  (utils.ex.nmap  [:ga "<Plug>(EasyAlign)"]))
