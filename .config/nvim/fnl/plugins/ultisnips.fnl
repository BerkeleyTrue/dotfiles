(module plugins.ultisnips
  {:require {utils utils}})


; We map the trigger initially to <c-w>
; so our <tab> keymaps are not overwritten
; by ultisnips.
; We then replicate the normal/x/select
; mode binding for ultisnips while
; keeping our own insert mode bindings
; This is not ideal but I'm unable to find another
; way.
(defn main []
  (utils.set-nvim-g!
    {:UltiSnipsExpandTrigger "<C-w>"
     :UltiSnipsJumpForwardTrigger "<C-b>"
     :UltiSnipsJumpBackwardTrigger "<C-c>"}))
