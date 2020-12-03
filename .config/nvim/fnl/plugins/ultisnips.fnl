(module plugins.ultisnips
  {:require {r r
             nvim aniseed.nvim}})


; We map the trigger initially to <c-w>
; so our <tab> keymaps are not overwritten
; by ultisnips.
; We then replicate the normal/x/select
; mode binding for ultisnips while
; keeping our own insert mode bindings
; This is not ideal but I'm unable to find another
; way.
(defn main []
  (->>
    {:UltiSnipsExpandTrigger "<C-w>"
     :UltiSnipsJumpForwardTrigger "<C-b>"
     :UltiSnipsJumpBackwardTrigger "<C-c>"}
    (r.to-pairs)
    (r.forEach
      (fn [[key val]] (tset nvim.g key val)))))
