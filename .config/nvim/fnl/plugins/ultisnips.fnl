(module plugins.ultisnips
  {:require {: utils}})


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
     :UltiSnipsJumpBackwardTrigger "<C-c>"
     :UltiSnipsRemoveSelectModeMappings 0})
  (let [(ok res) (pcall utils.ex.packadd :ultisnips)]
    (if (not ok)
      (print (.. "Could not load ultisnips: " res))
      (do
        (utils.snoremap :<tab> "<Esc>:call UltiSnips#ExpandSnippet()<cr>" {:silent true})
        (utils.xnoremap :<tab> ":call UltiSnips#SaveLastVisualSelection()<cr>gvs" {:silent true})
        (utils.inoremap :<C-j> "<C-R>=UltiSnips#JumpForwards()<cr>" {:silent true})
        (utils.snoremap :<C-j> "<Esc>:call UltiSnips#JumpForwards()<cr>" {:silent true})))))
