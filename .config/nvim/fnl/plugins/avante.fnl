(module plugins.avante
  {autoload
   {a aniseed.core
    r r}
   require 
   {avante avante}
   import-macros []
   require-macros [macros]})


(defn main []
  (avante.setup {}))
