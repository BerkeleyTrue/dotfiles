(module lib.corpus.chooser
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    previewer lib.corpus.previewer}
   require
   {Job plenary.job}
   require-macros [macros]})

(var buffer nil)
(var window nil)
(var current-job nil)
(var currently-selected nil)
(def namespace (n create_namespace "corpus-chooser"))

(defn- get-buffer []
  "Get the buffer for the chooser. If it doesn't exist, create it."
  (when (r.nil? buffer)
    (set buffer (n create_buf false true)))
  buffer)

(defn get-window []
  "Get the window for chooser. If it doesn't exist, create it."
  (when (r.nil? window)
    (let [buffer (get-buffer)
          width (/ (o columns) 2)]
      (set window (n open_win
                     buffer
                     false
                     {:col 0
                      :row 0
                      :focusable false
                      :relative :editor
                      :style :minimal
                      :width width
                      :height (- (o lines) 2)}))
      (n win_set_option window :wrap false)
      (n win_set_option window :winhl "Normal:Normal")))
  window)

(defn get-selected-file []
  "Get the currently selected file name in the buffer."
  (when (not= currently-selected nil)
    (let [lines (n buf_get_lines
                   buffer
                   (- currently-selected 1)
                   currently-selected
                   false)
          line (. lines 1)]
      (.. (vim.trim (line:sub 3 (length line))) :.md))))

(defn reset []
  (set currently-selected nil))

(defn highlight-selected [input]
  "Highlight the currently selected line in the buffer"
  (when (not= currently-selected nil)
    (n win_set_cursor window [currently-selected 0])
    (n buf_clear_namespace buffer namespace 0 -1)
    (n buf_add_highlight
       buffer
       namespace
       :PMenuSel
       (- currently-selected 1)
       0
       -1))
  (previewer.open (get-selected-file) input))

(defn next []
  "Move to the next item in the list"
  (when (and
          (not= currently-selected nil)
          (< currently-selected (n buf_line_count buffer)))
    (let [lines (n buf_get_lines
                   buffer
                   (- currently-selected 1)
                   (+ currently-selected 1)
                   false)
          line1 (string.gsub (. lines 1) "^.." "  ") ; returns two values, we only care about the first one
          line2 (string.gsub (. lines 2) "^.." "> ")
          lines [line1 line2]]
      (n buf_set_lines
         buffer
         (- currently-selected 1)
         (+ currently-selected 1)
         false
         lines)
      (set currently-selected (+ currently-selected 1))
      (highlight-selected))))

(defn prev []
  "Move to the previous item in the list"
  (when (and
          (not= currently-selected nil)
          (> currently-selected 1))
    (let [lines (n buf_get_lines
                   buffer
                   (- currently-selected 2)
                   currently-selected
                   false)
          line1 (string.gsub (. lines 1) "^.." "> ") ; returns two values, we only care about the first one
          line2 (string.gsub (. lines 2) "^.." "  ")
          lines [line1 line2]]
      (n buf_set_lines
         buffer
         (- currently-selected 2)
         currently-selected
         false
         lines)
      (set currently-selected (- currently-selected 1))
      (highlight-selected))))

(defn update [results input]
  "Update the list of files in the buffer"
  (let [buffer (get-buffer)
        window (get-window)
        width (o columns)
        selected-idx (if (r.empty? results) nil 1)
        _ (table.sort results)
        lines (->>
                results
                (r.to-pairs)
                (r.map
                  (fn [[idx val]]
                    (let [name (vf fnamemodify val ":r")
                          prefix (if (= selected-idx idx) "> " "  ")]

                      (if (< width 102)
                        (.. prefix (string.format (.. "%-" (- width 2) "s") name))
                        (let [padded (.. prefix (string.format "%-99s" name))
                              diff (- width (length padded))]
                          (if (> diff 0)
                            (.. padded (string.rep " " diff))
                            padded)))))))]
    (set currently-selected selected-idx)
    (n buf_set_lines
       buffer
       0
       -1
       false
       lines)
    (n win_set_height
       window
       (- vim.o.lines 2))
    (highlight-selected input)))

(defn ls []
  "Get a list of markdown files in the git repository and return them to the callback"
  (when (not= current-job nil)
    (current-job:shutdown))

  (set current-job
       (Job:new
         {:command :git
          :args [:ls-files :--cached :--others :-- :*.md]
          :on_exit
          (vim.schedule_wrap
            (fn list-on-exit [job]
              (let [results (job:result)]
                (update results))))}))
  (current-job:start))

(defn search [input cb]
  "Get a list of files in the directory using grep and return them to the callback"
  (when (not= current-job nil)
    (current-job:shutdown))
  (let [terms (->>
                input
                (r.lmatch "%S+")
                (r.join "|"))
        args [:--silent
              :--files-with-matches
              terms
              :.]] ; can't make it match only mardown files
    (set current-job
         (Job:new
           {:command :ag
            :args args
            :cwd "."
            :on_exit
            (vim.schedule_wrap
              (fn search-on-exit [job err data]
                (let [results (job:result)]
                  (update results input))))}))
    (current-job:start)))


(defn open [input]
  (get-window)
  (if (r.not-empty? input)
    (search input)
    (ls)))

(defn close []
  "Close the chooser window. If it exists, destroy it."
  (when (not= window nil)
    (n win_close window true)
    (set window nil))
  (previewer.close))
