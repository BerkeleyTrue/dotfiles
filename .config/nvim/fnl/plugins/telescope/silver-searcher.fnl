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
  "take each raw line input from Ag and transform it for telescope"
  (let [split (r.split entry ":")
        rel-path (r.head split)
        content (->> split (r.drop 3) (r.join ":"))
        abs-path (vim.loop.fs_realpath rel-path) ; ask vim to resolve the path for us
        line-num (tonumber (. split 2)) ; line number provided by Ag
        col-num (tonumber (. split 3))] ; column number provided by Ag
    {:value content ; the content of the file that matched the search
     :ordinal entry ; telescope search will filter against this
     :filename rel-path ; the filename TODO: get basename, currently relative path to CWD
     :path abs-path
     :lnum line-num ; line number in file
     :col col-num ; column number in file
     :display
     (fn make-display [display-entry]
       "function that will transform this entry into a line of text to dislay to the user,
        and a list of highlight groups to apply to that line"
       (let [(icon-display icon-hl-group) (tutils.transform_devicons (. display-entry :path)) ; get dev icons based on filename, and hl-group for icon
             path-display (tutils.transform_path {} (. display-entry :path)) ; format path for display, makes paths more uniform
             lnum-display (-> display-entry (. :lnum) (tostring)) ; convert line number to string
             col-display (-> display-entry (. :col) (tostring)) ; convert column number to string
             content-display (. display-entry :value)] ; grab the content of the entry
         (var display "") ; the display string we will build that will be shown to the user
         (var hl-groups []) ; the highlight groups we will apply to the display string
         (when (> (r.size icon-display) 0) ; sometimes devicons will not be found, so we need to check
           (set display (.. display icon-display))
           (table.insert hl-groups [[1 ; the starting char of the hl group
                                     (r.size icon-display)] ; the ending char of the hl group
                                    icon-hl-group])) ; the hilight group to apply

         (table.insert hl-groups [[(r.size display)
                                   (+
                                     (r.size display)
                                     (r.size path-display)
                                     1)]
                                  :BerksCyan])
         (set display (.. display " " path-display))
         (table.insert hl-groups [[(r.size display)
                                   (+
                                     (r.size display)
                                     (r.size lnum-display)
                                     1)]
                                  :BerksPurple])
         (set display (.. display ":" lnum-display))
         (table.insert hl-groups [[(r.size display)
                                   (+
                                     (r.size display)
                                     (r.size col-display)
                                     1)]
                                  :BerksYellow])
         (set display (.. display ":" col-display))
         (table.insert hl-groups [[(r.size display)
                                   (+
                                     (r.size display)
                                     (r.size content-display)
                                     1)]
                                  :Normal])
         (set display (.. display "|" content-display))
         (values display hl-groups)))})) ; use 'values' to return multiple values

(defn attach-mappings [prompt-bufnr]
  "attach mappings to the prompt buffer"
  (actions.select_default:replace
    (fn []
      (actions.close prompt-bufnr) ; close the prompt buffer
      (let [selection (astate.get_selected_entry)] ; get the selected entry, same as the object returned by entry-maker
        (vim.cmd ; we make a call to vim to open the file, and move the cursor to the line and column
          (..
            ":edit " (. selection :path) "\n"
            ; move cursor to line and column
            ; 44G4| -> line 44, column 4
            ":normal " (. selection :lnum) "G" (+ 1 (. selection :col)) "|")))))
  true)

(def- default-opts
  {:entry_maker entry-maker
   :attach_mappings attach-mappings})

(defn ag [...]
  "Run ag with the given args, and return the results as a telescope picker
  The last arg is taken as the directory to search in, or '.' if not provided
  The rest of the args are taken as the pattern to search for.
  If no args are provided, the word under the cursor is used as the pattern
  If the last arg is '.', the current buffers directory is used as the search directory
   unless the current buffer is not readable, i.e. a utility buffer, the current working
   directory is used.
  "
  (when-let [ag-not-found (not (= (vim.fn.executable "ag") 1))]
    (command echoe "'ag not found. Is silver searcher installed?'"))

  (let [args [...]
        args (if (r.empty? args) [(vim.fn.expand "<cword>")] args)]

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
           :finder (finders.new_oneshot_job args default-opts) ; this runs the ag command in a subshell
           :previewer (config.values.grep_previewer default-opts) ; this is the previewer for the results in telescope
           :sorter (config.values.file_sorter default-opts)})
        (: :find)))))


(defn main []
  (command!
    :Ag
    (viml->lua* ag {:args "<f-args>"})
    {:nargs :*
     :complete :file
     :desc "Silver search for files"}))
