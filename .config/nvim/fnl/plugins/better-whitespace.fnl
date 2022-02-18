(module plugins.better-whitespace
  {require {utils utils}})


(defn main []
  (utils.set-nvim-g!
    {:better_whitespace_enabled false
     :strip_whitespace_on_save true
     :strip_whitespace_confirm false
     :strip_whitelines_at_eof true
     :strip_only_modified_lines false}))
