(module lib.corpus
  {autoload
   {r r
    a aniseed.core
    utils utils
    md utils.module
    corpus corpus
    complete lib.corpus.complete
    cts lib.corpus.treesitter
    ftdetect lib.corpus.ftdetect
    metadata lib.corpus.metadata
    reflinks lib.corpus.reference-links
    shortcuts lib.corpus.shortcuts
    git lib.corpus.git}
   require {}
   require-macros [macros]})

(defn init-corpus [])

(defn main []
  (vim.treesitter.language.register :markdown :markdown.corpus)
  (augroup :LibCorpus
    {:event :VimEnter
     :pattern :*
     :callback
     (fn []
      (when (ftdetect.ftdetect)
        (command!
          :Corpus (fn [{: args : bang}]
                    (a.pr :args args)
                    (corpus.choose args bang))
          {:force true
           :desc "Choose a corpus file"
           :bang true
           :nargs "*"
           :complete complete.complete})))}

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
     (fn on-command-line-changed [{: file}] (corpus.cmdline_changed file))}

    {:event [:CmdlineLeave]
     :pattern :*
     :callback
     (fn on-command-line-leave []
       (corpus.cmdline_leave))}

    {:event [:BufNewFile :BufRead]
     :pattern :*.md
     :callback
     (fn []
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
                (git.commit file "update")))})))}))
