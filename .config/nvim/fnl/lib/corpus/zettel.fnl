(module lib.corpus.zettel
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    metadata lib.corpus.metadata}
   require
   {Input nui.input}
   require-macros [macros]})

(def- lib-augroup :LibCorpusZet)
(def- zettel-dir "00-zk")

(def- popup-opts
  {:border {:style :rounded
            :text {:top "[New Zettel Title]"
                   :top_align :left}}
   :position "50%"
   :relative :editor
   :focusable true
   :size 100})


(defn is-temp-zet? [filename]
  "Returns true if the filename is a temp zettel"
  (let [tempid (vf fnameescape (.. "./" zettel-dir "/temp-"))]
    (r.starts-with? filename tempid)))

(defn handle-write []
  "Before writing to file, prompt the user for a title."
  (fn handle-submit [title]
    (let [bufnr (n get-current-buf)]
      (if (> (length title) 3)
        (let [filename (vf fnameescape (.. zettel-dir "/" (r.kebab-case title) ".md"))]
          (n del-augroup-by-name lib-augroup)
          (n buf-set-name 0 filename)
          (metadata.update-file {:force? true})
          (command "write"))
        (vim.print "Title too short."))))

  (doto
    (Input popup-opts
           {:prompt "> "
            :default_value ""
            :on_submit handle-submit})
    (: :mount)))

(comment (handle-write))

(defn ensure-zet-dir []
  "Ensure the zettel directory exists"
  (when-let [exists? (= (vf glob zettel-dir) "")]
    (vf mkdir zettel-dir)))

(defn create []
  "Create a new temp buffer under the zettel directory
  Sets the filename to a temp name.
  Before writing to file, the user is prompted for a title.
  The title is kebab-cased and used as the filename."
  (ensure-zet-dir)
  (let [tempid (vf fnameescape (.. zettel-dir "/" "temp-" (math.random) ".md"))]
    (vim.cmd (.. "edit " tempid))
    (metadata.update-file {:force? true :temp? true})
    (augroup lib-augroup
      {:event :BufWriteCmd
       :buffer 0
       :callback (vim.schedule_wrap handle-write)})))
