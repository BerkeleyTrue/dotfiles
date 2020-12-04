(module plugins.delimit-mate
  {:require {utils utils}})

(defn main []
  (utils.set-nvim-g! {
                      ; When opening a pair of surround and hitting <CR>
                      ; this will expand the pair onto new lines
                      :delimitMate_expand_cr 2
                      ; Same as above but will add padding to surround
                      :delimitMate_expand_space 1
                      ; Allow inserting closing surround on expansion
                      ; to jump to the already existing closing
                      ; surround instead of inserting a new closing surround
                      :delimitMate_jump_expansion 1}))
