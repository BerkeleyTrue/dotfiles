(module lib.corpus.chooser
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    previewer lib.corpus.previewer
    {: run} lib.spawn}
   require-macros [macros]})

(var buffer nil)
(var window nil)
(var current-job nil)
(var currently-selected nil)
(def namespace (n create_namespace "corpus-chooser"))

(defn get-width []
  "Get the width of the chooser window."
  (->
    (o columns)
    (/ 2)
    (math.floor)
    (math.max 0)))

(defn- get-buffer []
  "Get the buffer for the chooser. If it doesn't exist, create it."
  (when (r.nil? buffer)
    (set buffer (n create_buf false true)))
  buffer)

(defn get-window []
  "Get the window for chooser. If it doesn't exist, create it."
  (when (r.nil? window)
    (let [buffer (get-buffer)
          width (get-width)]
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
  "Get the currently selected file name in the chooser buffer."
  (when-not (r.nil? currently-selected)
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
        width (get-width)
        selected-idx (if (r.empty? results) nil 1)
        _ (table.sort results) ; in place sort 🤮
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
                            padded))))))
                (r.map (fn [s] (string.gsub s "\n" ""))))]
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
    (current-job))

  (set current-job
       (run
         {:command :git
          :args [:ls-files :--cached :--others :-- :*.md]}
         (fn list-on-exit [ok results]
           (when ok
             (update results))))))

(defn search [input cb]
  "Get a list of files in the directory using grep and return them to the callback"
  (when (not= current-job nil)
    (current-job))
  (let [terms (->>
                input
                (r.lmatch "%S+")
                (r.join "|"))
        args [:--silent
              :--files-with-matches
              terms
              :.]] ; can't make it match only mardown files
    (set current-job
         (run
           {:command :ag
            :args args
            :cwd "."}
           (fn search-on-exit [ok results]
             (when ok
               (update results input)))))))


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
