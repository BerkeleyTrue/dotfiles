(module plugins.auto-session
  {autoload
   {a aniseed.core
    r r
    as auto-session
    md utils.module 
    conjlog conjure.log
    neotree neo-tree.command}
   require {}
   import-macros []
   require-macros [macros]})

(defn main []
  (as.setup 
    {:pre_save_cmds 
     [(fn close-conjure []
        (conjlog.close-visible))
      (fn close-neotree []
        (neotree.execute {:action :close}))]})) 
