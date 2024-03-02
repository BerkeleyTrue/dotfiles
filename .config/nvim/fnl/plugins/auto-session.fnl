(module plugins.auto-session
  {autoload
   {a aniseed.core
    r r
    as auto-session}
   require {}
   import-macros []
   require-macros [macros]})

(defn main []
  (as.setup {})) 
