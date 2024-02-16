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
  (nnoremap "<C-n>" #(mc.add_cursor_and_jump_to_next_match))
  (xnoremap "<C-n>" #(mc.add_cursor_and_jump_to_next_match))
  (nnoremap "<leader>q" #(mc.jump_to_next_match)))

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
