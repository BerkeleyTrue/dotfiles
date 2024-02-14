(module lib.corpus.chooser
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    previewer lib.corpus.previewer
    Job plenary.job}
   require {}
   require-macros [macros]})

(var buffer nil)
(var window nil)
(var current-job nil)
(var currently-selected nil)
(def namespace (n create_namespace "corpus-chooser"))

(defn- get-buffer []
  "Get the buffer for the chooser. If it doesn't exist, create it."
  (when (r.nil? buffer)
    (set buffer (n vim_create_buffer false true)))
  buffer)

(defn open []
  "Open the chooser window. If it doesn't exist, create it."
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

(defn close []
  "Close the chooser window. If it exists, destroy it."
  (when (not (r.nil? window))
    (n vim_close window true)
    (set window nil)))

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

(defn highlight-selected []
  "Highlight the currently selected line in the buffer"
  (when (not= currently-selected nil)
    (n win_set_cursor window [currently-selected 0])
    (n buf_clear_namespace
       buffer
       namespace
       0
       -1)
    (n buf_add_highlight
       buffer
       namespace
       :PMenuSel
       (- currently-selected 1)
       0
       -1))
  (previewer.show (get-selected-file)))

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
          lines [(: (. lines 1) :gsub "^.." "  ")
                 (: (. lines 2) :gsub "^.." "> ")]]
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
          lines [(: (. lines 1) :gsub "^.." "> ")
                 (: (. lines 2) :gsub "^.." "  ")]]
      (n buf_set_lines
         buffer
         (- currently-selected 2)
         currently-selected
         false
         lines)
      (set currently-selected (- currently-selected 1))
      (highlight-selected))))

(defn list [cb]
  "Get a list of markdown files in the git repository and return them to the callback"
  (when (not= current-job nil)
    (current-job:shutdown))

  (set current-job
       (Job:new
         {:command :git
          :args [:ls-files :--cached :--others :-- :*.md]
          :on_exit
          (fn [job]
            (let [results (job:results)]
              (cb results)))})))

(defn search [input cb]
  "Get a list of files in the directory using grep and return them to the callback"
  (when (not= current-job nil)
    (current-job:shutdown))
  (let [args [:--no-heading
              :--smart-case
              :--fixed
              :--silent
              :--files-with-matches
              :--untracked
              (.. "\"" input "\"")
              :*.md]]
    (set current-job
         (Job:new
           {:command :ag
            :args args
            :on_exit
            (fn [job]
              (let [results (job:results)]
                (cb results)))}))))

(defn update []
  "Update the list of files in the buffer")
