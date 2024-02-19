(module plugins.multi-cursor
  {autoload
   {a aniseed.core
    r r
    utils utils
    md utils.module
    mc multiple-cursors}
   require-macros [macros]})

(var lens-bak nil)

(defn init []
  (nnoremap "<C-n>" (fn multi-cursor-add-n-jump [] (mc.add_cursor_and_jump_to_next_match)) {:desc "Add cursor and jump to next match"})
  (xnoremap "<C-n>" (fn multi-cursor-add-n-jump [] (mc.add_cursor_and_jump_to_next_match)) {:desc "Add cursor and jump to next match"})
  (nnoremap "<leader>q" (fn multi-cursor-skip-match [] (mc.jump_to_next_match)) {:desc "Skip to next match"}))

(defn main []
  (mc.setup
    {:pre_hook
     (fn []
       (g! minipairs_disable true)
       (b! visual_multi true)
       (command NoMatchParen))
     :post_hook
     (fn []
       (g! minipairs_disable false)
       (b! visual_multi false)
       (command DoMatchParen))}))
