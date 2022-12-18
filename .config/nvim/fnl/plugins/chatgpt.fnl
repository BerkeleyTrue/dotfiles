(module plugins.chatgpt
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [gpt (md.prequire :chatgpt)]
    (gpt.setup
      {:welcome_message "Hello"})))
