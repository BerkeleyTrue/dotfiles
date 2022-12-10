(module plugins.easy-align
  {require
   {md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (xmap :ga "<Plug>(EasyAlign)")
  (nmap :ga "<Plug>(EasyAlign)"))
