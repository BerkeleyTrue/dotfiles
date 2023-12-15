(module plugins.zenmode
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def conf {:plugins {:kitty {:enable true}}})

(defn main []
  (let [zm (md.prequire :zen-mode)]
    (zm.setup conf)
    (command!
      :ZenModeOnly
      (fn zen-mode-only []
        (zm.setup
          (r.merge
            conf
            {:on_open
             (fn on-open []
               (vim.cmd "cabbrev <buffer> q let b:quitting = 1 <bar> q")
               (vim.cmd "cabbrev <buffer> wq let b:quitting = 1 <bar> wq"))
             :on_close
             (fn on-close []
               (when (= (b quitting) 1)
                 (b! quitting 0)
                 (vim.cmd :q)))}))
        (zm.open)))))
