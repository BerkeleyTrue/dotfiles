(module plugins.telescope
  {:require {a aniseed.core
             telescope telescope
             actions telescope.actions
             sorters telescope.sorters
             themes telescope.themes}})



(defn- setup [telescope]
  (telescope.setup
    {:defaults {}
     :borderchars {
                   1 ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]
                   :preview ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]}

     :sorting_strategy "descending"
     :prompt_position "bottom"
     :color_devicons true
     :file_sorter sorters.get_fzy_sorter}))

(setup telescope)
