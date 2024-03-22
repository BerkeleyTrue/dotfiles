(module lib.corpus
  {autoload
   {r          r
    a          aniseed.core
    utils      utils
    md         utils.module
    path       lib.path
    cts        lib.corpus.treesitter
    ftdetect   lib.corpus.ftdetect
    metadata   lib.corpus.metadata
    reflinks   lib.corpus.reference-links
    shortcuts  lib.corpus.shortcuts
    git        lib.corpus.git
    chooser    lib.corpus.chooser
    zet        lib.corpus.zettel}
   require {}
   require-macros [macros]})

(defn preview-mappings []
  (cnoremap "<C-j>"    #(chooser.next)     {:silent true :buffer true :desc "Corpus: Go to next item"})
  (cnoremap "<C-k>"    #(chooser.prev)     {:silent true :buffer true :desc "Corpus: Go to previous item"})
  (cnoremap "<Down>"   #(chooser.next)     {:silent true :buffer true :desc "Corpus: Go to next item"})
  (cnoremap "<Up>"     #(chooser.prev)     {:silent true :buffer true :desc "Corpus: Go to previous item"}))

(defn complete [arglead cmdline _]
  (when-let [file (chooser.get-selected-file)]
    (let [title (file:sub 1 -4) ; remove .md
          (prefix _) (cmdline:gsub "^%s*Corpus!?%s+" "")] ; remove prefix
      (when (vim.startswith title prefix) ; if title starts with prefix
        ;; If on "foo bar bazzzz"
        ;;                   ^
        ;; Must return "bazzzz", not "zzz".
        [(title:sub
           (+ (- (prefix:len)
                 (arglead:len))
              1)
           -1)]))))

(defn choose [selection bang]
  "Open the selected file in a new buffer. If the file does not exist, create it.
  If the selection ends with a bang, create the file.
  If the selection does not end with a bang, use the selected file.
  If no file is selected, use the selection as the file name."
  (let [selection (vim.trim selection)
        create (or (= bang "!") (r.ends-with? selection "!"))
        selection (if (r.ends-with? selection "!") (selection:sub 0 -2) selection)

        filename (if create
                   selection ; if create, use selection as is
                   (chooser.get-selected-file)) ; else, get selected file in the chooser buffer

        filename (if (and
                        (r.not-empty? filename)
                        (not (r.ends-with? filename :.md))) ; and not .md
                   (.. filename :.md) ; add file ext
                   filename)
        filename (r.kebab-case filename)
        filename (vf fnameescape filename)]

    (command edit filename)
    (metadata.update-file {:force? true})))

(defn cmdline-changed [char file]
  "When the command line changes, check if it's a corpus command
  and if so, show the chooser and previewer."
  (when (= char ":")
    (let [line (vim.fn.getcmdline)
          (_ _ preterm term) (string.find line "^Corpus%f[%A](%s*)(.-)%s*$")]
      (if (and (r.not-empty? preterm) ; space after corpus, intent to search
               (ftdetect.ftdetect file))
        (do
          (preview-mappings)
          (chooser.open term))
        (chooser.close)))))

(comment (string.find "Corpus bar" "^Corpus%f[%A](%s*)(.-)%s*$"))

(defn init [{: file}]
  (when (ftdetect.ftdetect file)
    (bo! filetype "markdown.corpus")
    (nnoremap "<C-]>" #(shortcuts.go-to-or-create-shortcut) {:silent true :buffer true :desc "Corpus: Go to or create shortcut"})
    ; have to use a caommand here because of the bang
    ; https://github.com/neovim/neovim/issues/18340
    ; where keymap callbacks aren't given 
    (xnoremap "<C-]>" ":lua require('lib.corpus.shortcuts')['create-shortcut-on-selection']()<CR>" {:silent true :buffer true :desc "Corpus: Create shortcut on selection"})

    (augroup :LibCorpusEnv
      {:event [:BufWritePre]
       :buffer 0
       :callback
       (fn before-write [{: file}]
         (reflinks.update-file)
         (metadata.update-file))}

      {:event [:BufWritePost]
       :buffer 0
       :callback
       (fn after-write [{: file}]
         (when-not (zet.is-temp-zet? file)
           (let [root (ftdetect.get-corpus-root file)
                 file (path.get-relative-path root file)
                 file (string.sub file 2)]
             ((git.commit file root)))))})))

(defn main []
  (vim.treesitter.language.register :markdown :markdown.corpus)
  (augroup :LibCorpus
    {:event :VimEnter
     :pattern :*
     :callback
     (fn vim-enter [{: file}]
       (when (ftdetect.ftdetect (or file (vf expand "%")))
         (command!
           :Corpus
           (fn on-corpus-command [{: args : bang}]
             (choose args bang))
           {:desc "Choose a corpus file"
            :bang true
            :nargs "*"
            :complete complete})

         (command! :CorpusRefLinks #(reflinks.update-file))
         (command! :CorpusMetaData #(metadata.update-file {:force? true}))
         (command! :CorpusAddTag (fn [{: fargs}]
                                   (metadata.update-file
                                     {:tags fargs 
                                      :force? true})) 
                   {:nargs :*
                    :desc "Add tags to the current file"
                    :complete #[:reference :video :book :article :podcast :course :other]})

         (command!
           :Zet
           #(zet.create)
           {:desc "Create a new temp zettel note"})))}

    {:event [:CmdlineEnter]
     :pattern :*
     :callback
     (fn on-command-line-enter []
       (chooser.reset))}

    {:event [:CmdlineChanged]
     :pattern :*
     :callback
     (fn on-command-line-changed [{: file}]
       (let [char file
             file (vf expand "%:p")]
         (cmdline-changed char file)))}

    {:event [:CmdlineLeave]
     :pattern :*
     :callback
     (fn on-command-line-leave []
       (chooser.close))}

    {:event [:BufNewFile :BufRead]
     :pattern :*.md
     :callback init}))
