(module plugins.avante
  {autoload
   {a aniseed.core
    r r}
   require 
   {avante avante}
   import-macros []
   require-macros [macros]})


(defn main []
  (avante.setup 
    {:claude 
     {:api_key_name (.. "cmd:cat " (os.getenv "HOME") "/.anthropic_api_key")
      :model :claude-3-7-sonnet-20250219}
     :windows
     {:position "left"}}))
