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
  (let [tempid (vf fnameescape "temp-")]
    (r.starts-with? filename tempid)))

(defn handle-write []
  "Before writing to file, prompt the user for a title."
  (fn handle-submit [title]
    (let [bufnr (n get-current-buf)]
      (if (> (length title) 3)
        (let [filename (vf fnameescape (.. (r.kebab-case title) ".md"))]
          (n del-augroup-by-name lib-augroup)
          (n buf-set-name 0 filename)
          (metadata.update-file {:force? true 
                                 :tags [:zettel]})
          (command "write"))
        (vim.print "Title too short."))))

  (doto
    (Input popup-opts
           {:prompt "> "
            :default_value ""
            :on_submit handle-submit})
    (: :mount)))

(comment (handle-write))

(defn create []
  "Create a new temp buffer.
  Sets the filename to a temp name.
  Before writing to file, the user is prompted for a title.
  The title is kebab-cased and used as the filename."
  (let [tempid (vf fnameescape (.. "temp-" (math.random) ".md"))]
    (vim.cmd (.. "edit " tempid))
    ; allow treesitter to parse file
    (vim.schedule #(metadata.update-file {:force? true :temp? true :tags [:zettel]}))
    (augroup lib-augroup
      {:event :BufWriteCmd
       :buffer 0
       :callback (vim.schedule_wrap handle-write)})))
