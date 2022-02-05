(module plugins.easy-align
  {require
   {md utils.module
    utils utils}})

(defn main []
  (md.ppackadd :vim-easy-align)
  (utils.ex.xmap  [:ga "<Plug>(EasyAlign)"])
  (utils.ex.nmap  [:ga "<Plug>(EasyAlign)"]))
