(module plugins.auto-session
  {autoload
   {a aniseed.core
    r r
    as auto-session
    md utils.module 
    conjlog conjure.log}
   require {}
   import-macros []
   require-macros [macros]})

(defn main []
  (as.setup 
    {:pre_save_cmds 
     [(fn close-conjure []
        (conjlog.close-visible))]})) 
