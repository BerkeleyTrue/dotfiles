(module plugins.better-whitespace
  {:require {utils utils}})


(defn main []
  (utils.set-nvim-g! {:better_whitespace_enabled 0
                      :strip_whitespace_on_save 1
                      :strip_whitespace_confirm 0
                      :strip_only_modified_lines 1}))
