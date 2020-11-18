(module plugins.indent-guides
  {:require {a aniseed.core
             ig indent_guides}})



(do
  (tset ig :options {:indent_guide_size 1
                     :indent_start_level 2
                     :indent_levels 30
                     :indent_space_guides true
                     :indent_tab_guides true
                     :indent_soft_pattern "\\s"
                     :exclude_filetypes [:help]}))
