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
        abs-path (vim.loop.fs_realpath rel-path)
        line-num (tonumber (r.head (r.tail split)))]
    {:value entry
     :ordinal rel-path
     :filename rel-path
     :path abs-path
     :lnum line-num
     :display
     (fn [display-entry]
       (let [display (tutils.transform_path {} (. display-entry :value))
             split (r.split display ":")
             fn-length (-> split (r.head) (length))
             ln-length (-> split (r.tail) (r.head) (length))
             col-length (-> split (r.tail) (r.tail) (r.head) (length))
             (display hl-group) (tutils.transform_devicons (. display-entry :path) display false)]
         (if hl-group
           (values display [[[1 3]
                             hl-group]
                            [[5 (+ 5 fn-length)]
                             :BerksCyan]
                            [[(+ 6 fn-length) (+ 6 fn-length ln-length)]
                             :BerksPurple]
                            [[(+ 7 fn-length ln-length) (+ 7 fn-length ln-length col-length)]
                             :BerksYellow]])
           (values display [[[1 fn-length]
                             :BerksCyan]
                            [[fn-length (+ 6 fn-length ln-length)]
                             :BerksPurple]
                            [[(+ 7 fn-length ln-length) (+ 7 fn-length ln-length col-length)]
                             :BerksYellow]]))))}))

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
    (command echoe "'ag not found. Is silver searcher installed?'"))

  (let [args [...]
        args (if (r.empty? args) [(vim.fn.expand "<cword>")] args)]

    (if (r.empty? args)
      (command echoe "'Usage: \":Ag {pattern}\" (or just :Ag to search for the word under the cursor).'")
      (let [pattern (if (> (length args) 1) (r.join "" (r.initial args)) (r.head args))
            last (if (> (length args) 2) (r.last args) ".")
            dir (if
                  (= last ".")
                  ; if current buffer is a file, use its directory
                  ; otherwise use current working directory
                  (if (vim.fn.filereadable (vim.fn.expand "%"))
                    (vim.fn.expand "%:p:h")
                    (vim.fn.getcwd))
                  last)
            args [:ag :--vimgrep :--nocolor :--noheading pattern dir]]
        (doto
          (pickers.new
            default-opts
            {:prompt_title :Ag
             :finder (finders.new_oneshot_job args default-opts)
             :previewer (config.values.grep_previewer default-opts)
             :sorter (config.values.file_sorter default-opts)})
          (: :find))))))


(defn main []
  (command!
    :Ag
    (viml->lua* ag {:args "<f-args>"})
    {:nargs :*
     :complete :file
     :desc "Silver search for files"}))
