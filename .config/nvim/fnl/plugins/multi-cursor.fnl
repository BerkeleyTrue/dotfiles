(module plugins.multi-cursor
  {require {utils utils}})

(defn main []
  (utils.set-nvim-g! {:multi_cursor_exit_from_insert_mode 0
                      :multi_cursor_exit_from_visual_mode 0}))
