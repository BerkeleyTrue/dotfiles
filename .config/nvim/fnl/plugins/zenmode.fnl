(module plugins.zenmode
  {autoload
   {a aniseed.core
    r r
    zm zen-mode
    tw twilight}
   require-macros [macros]})

(def conf
  {:plugins {:options {:laststatus 3
                       :ruler true}
             :gitsigns {:enabled true}
             :twilight {:enabled false}}})

(defn main []
  (zm.setup conf)
  (nnoremap :<leader>uz #(zm.toggle)) ; u for ui
  (nnoremap :<leader>ut #(tw.toggle))
  (command!
    :ZenModeOnly
    (fn zen-mode-only []
      (zm.setup
        (r.merge
          conf
          {:plugins {:kitty {:enabled true
                             :font "+4"}
                     :twilight {:enabled true}}
           :on_open
           (fn on-open []
             (vim.cmd "cabbrev <buffer> q let b:quitting = 1 <bar> q")
             (vim.cmd "cabbrev <buffer> wq let b:quitting = 1 <bar> wq"))
           :on_close
           (fn on-close []
             (when (= (b quitting) 1)
               (b! quitting 0)
               (vim.cmd :q)))}))
      (zm.open))))
