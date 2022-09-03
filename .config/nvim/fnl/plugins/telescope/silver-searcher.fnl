(module plugins.telescope.silver-searcher
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils

    pickers telescope.pickers
    finders telescope.finders
    config  telescope.config
    actions telescope.actions
    astate  telescope.actions.state
    tutils  telescope.utils}

   require-macros [macros]})


(defn entry-maker [entry]
  (let [split (vim.split entry ":")
        rel-path (r.head split)
        abs-path (.. (vim.fn.getcwd) "/" rel-path)
        line-num (tonumber (r.head (r.tail split)))]
    {:value entry
     :ordinal rel-path
     :filename rel-path
     :path abs-path
     :lnum line-num
     :display
     (fn [display-entry]
       (let [display (tutils.transform_path {} (. display-entry :value))
             (display hl-group) (tutils.transform_devicons (. display-entry :path) display false)]
         (if hl-group
           (values display [[[1 3] hl-group]])
           display)))}))

(defn attach-mappings [prompt-bufnr]
  (actions.select_default:replace
    (fn []
      (actions.close prompt-bufnr)
      (let [selection (astate.get_selected_entry)]
        (vim.cmd (.. ":e +" (. selection :lnum) " " (. selection :path))))))
  true)

(def- default-opts
  {:entry_maker entry-maker
   :attach_mappings attach-mappings})

(defn ag [...]
  (when-let [ag-not-found (not (= (vim.fn.executable "ag") 1))]
    (utils.ex.echoe "'ag not found. Is silver searcher installed?'"))

  (let [args [...]
        args (if (r.empty? args) (vim.fn.expand "<cword>") args)]

    (if (r.empty? args)
      (utils.ex.echoe "'Usage: \":Ag {pattern}\" (or just :Ag to search for the word under the cursor).'")
      (let [args (r.concat [:ag (r.head args)] (r.tail args))]
        (: (pickers.new
            default-opts
            {:promp_title :Ag
             :finder (finders.new_oneshot_job args default-opts)
             :previewer (config.values.grep_previewer default-opts)
             :sorter (config.values.file_sorter default-opts)})
         :find)))))

(defn main []
  (command!
    :Ag
    (viml->lua* ag {:args "<f-args>"})
    {:nargs :*
     :desc "Silver search for files"}))
