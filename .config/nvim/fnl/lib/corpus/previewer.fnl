(module lib.corpus.previewer
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(var buffer nil)
(var window nil)

(defn- get-buffer []
  (when (= buffer nil)
    (set buffer (n create_buf false true)) ; scratch
    (n buf_set_option buffer :filetype :markdown))
  buffer)

(defn- get-window []
  (when (= window nil)
    (let [width (math.floor (/ (o columns) 2))
          lines (o lines)]
      (set window (n open_win
                   (get-buffer)
                   false ; enter
                   {:col width
                    :row 0
                    :focusable false
                    :relative :editor
                    :style :minimal
                    :width width
                    :height (- lines 2)}))
      (n win_set_option window :winhighlight "EndOfBuffer:LineNr,FoldColumn:StatusLine,Normal:LineNr")
      (n win_set_option window :foldcolumn :1)
      (n win_set_option window :foldenable false)))
  window)

(defn search-contents [input]
  (when (r.not-empty? input)
    (let [term (->>
                 input
                 (r.lmatch "%S+")
                 (r.join "\\|"))]
      (n buf_call (get-buffer) #(vim.api.nvim_command (.. "silent! /" term))))))

(defn open [file input]
  (let [buf (get-buffer)
        win (get-window)
        lines (o lines)
        contents (if (r.nil? file) {} (vf readfile file "" lines))
        padding (- lines (length contents) 2)
        contents (r.concat contents
                           (->> (r.range 1 padding)
                                (r.map #"")))]
    (n buf_set_lines
       buf
       0 ; start
       -1 ; end
       false ; strict_indexing
       contents)
    (search-contents input)
    (vim.cmd :redraw)))

(defn close []
  (when (not (r.nil? window))
    (n win_close window true) ; force
    (set window nil)))
