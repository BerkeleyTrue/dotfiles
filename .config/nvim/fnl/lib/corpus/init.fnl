(module lib.corpus
  {autoload
   {r r
    a aniseed.core
    utils utils
    md utils.module
    corpus corpus
    chooser corpus.private.chooser
    cts lib.corpus.treesitter
    ftdetect lib.corpus.ftdetect
    metadata lib.corpus.metadata
    reflinks lib.corpus.reference-links
    shortcuts lib.corpus.shortcuts
    git lib.corpus.git}
   require {}
   require-macros [macros]})

(defn complete [arglead cmdline _]
  (when-let [file (chooser.get_selected_file)]
    (let [title (file:sub 1 -4) ; remove .md
          (prefix _) (cmdline:gsub "^%s*Corpus!?%s+" "")] ; remove prefix
      (when (vim.startswith title prefix) ; if title starts with prefix
        ;; If on "foo bar bazzzz"
        ;;                   ^
        ;; Must return "bazzzz", not "zzz".
        [(title:sub
           (+ (- (prefix:len) (arglead:len)) 1)
           -1)]))))

(defn init []
  (when (ftdetect.ftdetect)
    (noremap "<C-]>" (fn [] (shortcuts.go-to-or-create-shortcut)) {:silent true :buffer true})
    (xnoremap "<C-]>" (fn [] (shortcuts.create-shortcut-on-selection)) {:silent true :buffer true})
    (augroup :LibCorpusEnv
      {:event [:BufWritePre]
       :buffer 0
       :callback
       (fn before-write []
         (reflinks.update-file)
         (metadata.update-file))}

      {:event [:BufWritePost]
       :buffer 0
       :callback
       (fn after-write [{: file}]
         (if (b corpus_new_file)
           (do
             (b! corpus_new_file false)
             (git.commit file "create"))
           (git.commit file "update")))})))

(defn main []
  (vim.treesitter.language.register :markdown :markdown.corpus)
  (augroup :LibCorpus
    {:event :VimEnter
     :pattern :*
     :callback
     (fn []
      (when (ftdetect.ftdetect)
        (command!
          :Corpus
          (fn on-corpus-command [{: args : bang}]
            (corpus.choose args bang))
          {:force true
           :desc "Choose a corpus file"
           :bang true
           :nargs "*"
           :complete complete})))}

    {:event [:BufNewFile]
     :pattern :*.md
     :callback
     (fn buf-new-file [{: file}]
       (when (ftdetect.ftdetect)
         (metadata.update-file file)
         (b! corpus_new_file true)))}

    {:event [:CmdlineEnter]
     :pattern :*
     :callback
     (fn on-command-line-enter []
       (corpus.cmdline_enter))}

    {:event [:CmdlineChanged]
     :pattern :*
     :callback
     (fn on-command-line-changed [{: file}]
       (corpus.cmdline_changed file))}

    {:event [:CmdlineLeave]
     :pattern :*
     :callback
     (fn on-command-line-leave []
       (corpus.cmdline_leave))}

    {:event [:BufNewFile :BufRead]
     :pattern :*.md
     :callback init}))
