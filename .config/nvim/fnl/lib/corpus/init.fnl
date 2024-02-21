(module lib.corpus
  {autoload
   {r          r
    a          aniseed.core
    utils      utils
    md         utils.module
    cts        lib.corpus.treesitter
    ftdetect   lib.corpus.ftdetect
    metadata   lib.corpus.metadata
    reflinks   lib.corpus.reference-links
    shortcuts  lib.corpus.shortcuts
    git        lib.corpus.git
    chooser    lib.corpus.chooser}
   require {}
   require-macros [macros]})

(defn preview-mappings []
  (cnoremap "<C-j>"    #(chooser.next)     {:silent true :buffer true})
  (cnoremap "<C-k>"    #(chooser.prev)     {:silent true :buffer true})
  (cnoremap "<Down>"   #(chooser.next)     {:silent true :buffer true})
  (cnoremap "<Up>"     #(chooser.prev)     {:silent true :buffer true}))

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
  (let [selection (vim.trim selection)
        create (or (= bang "!") (vim.endswith selection "!"))
        selection (if (vim.endswith selection "!") (selection:sub 0 -2) selection)
        file (if create
               selection ; if create, use selection as is
               (chooser.get-selected-file)) ; else, get selected file TODO: how does this work?
        file (if (and
                   (not= file "") ; if file is not empty
                   (not= file nil) ; and not nil
                   (not (vim.endswith file :.md))) ; and not .md
               (.. file :.md) ; add .md
               file)]
    (vim.cmd (.. "edit " (vim.fn.fnameescape file)))))

(defn cmdline-changed [char]
  (when (= char ":")
    (let [line (vim.fn.getcmdline)
          (_ _ term) (string.find line "^%s*Corpus%f[%A]%s*(.-)%s*$")]
      (when (and (not= term nil) ; can be empty string, just not nil
                 (ftdetect.ftdetect))
        (preview-mappings)
        (chooser.open term)))))

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
         ((git.commit file)))})))

(defn main []
  (vim.treesitter.language.register :markdown :markdown.corpus)
  (augroup :LibCorpus
    {:event :VimEnter
     :pattern :*
     :callback
     (fn vim-enter []
      (when (ftdetect.ftdetect)
        (command!
          :Corpus
          (fn on-corpus-command [{: args : bang}]
            (choose args bang))
          {:force true
           :desc "Choose a corpus file"
           :bang true
           :nargs "*"
           :complete complete})))}

    {:event [:CmdlineEnter]
     :pattern :*
     :callback
     (fn on-command-line-enter []
       (chooser.reset))}

    {:event [:CmdlineChanged]
     :pattern :*
     :callback
     (fn on-command-line-changed [{: file}]
       (cmdline-changed file))}

    {:event [:CmdlineLeave]
     :pattern :*
     :callback
     (fn on-command-line-leave []
       (chooser.close))}

    {:event [:BufNewFile :BufRead]
     :pattern :*.md
     :callback init}))
