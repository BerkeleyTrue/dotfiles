(module lib.corpus.shortcuts
  {autoload
   {a aniseed.core
    r r
    utils utils
    ts lib.corpus.treesitter
    rl lib.corpus.reference-links
    md lib.corpus.metadata}
   require {}
   require-macros [macros]})

(defn create-shortcut-on-word []
  (let [word (vf expand "<cword>")
        linenm (vf line ".")
        start (vf search (.. "\\<" (vf escape word "\\") "\\>") :bcn)
        end (+ start (- (length word) 1))
        link (.. "[" word "]")
        line (vf getline linenm)
        linkified (vf substitute line (.. "\\<" (vf escape word "\\") "\\>") link "g")]
    (vf setline linenm linkified)
    ; give tree sitter a chance to update the syntax tree
    (vim.schedule rl.update-file)))

(defn create-shortcut-on-selection []
  "Create a shortcut on the selected text"
  (let [start (vf getpos "'<") ; bufnm, lnum, col, off
        end (vf getpos "'>")
        linenm (vf line "v")] ;
    (when (and
            ; must be single line selection
            (= (. start 2) (. end 2))
            ; must not be visual line selection
            (not (= (. end 3) 2147483647))
            ; must not be an empty selection
            (not (= (- (- (. start 3) 1) (- (. end 3) 1)) 0)))
      (let [line (vf getline linenm)
            start (- (. start 3) 1)
            end (- (. end 3) 1)
            len (length line)
            words (r.kebab-case (vf strpart line start (+ (- end start ) 1)))
            prefix (vf strpart line 0 start)
            postfix (vf strpart line (+ end 1) (- len end))
            linkified (.. prefix "[" words "]" postfix)]
        (vf setline linenm linkified)
        (vf cursor 0 (+ end 3))
        (vim.schedule rl.update-file)))))

(comment
  (vnoremap "<C-]>" create-shortcut-on-selection {:buffer true :silent true}))

(defn link? []
  "Checks if the node under the cursor is a shortcut or a reference link"
  (if-let [{:type _type : text} (ts.get-node-under-cursor)]
    (or (= _type :link_text)
        (= _type :link_label))
    false))

(comment
  (command! :CorpusIsShortcut (fn [] (a.println (link?)))))

(defn go-to-shortcut []
  "Go to the shortcut under the cursor"
  (let [{: text :type _type} (ts.get-node-under-cursor)
        text (if (= _type :link_text) text (r.lsub "%[(.*)%]" "%1" text))
        first-letter (r.head text)
        rest (r.tail text)
        target (.. "./" (r.kebab-case text) ".md")
        glob (.. "./[" (string.lower first-letter) (string.upper first-letter) "]" (r.kebab-case rest) ".md")
        mtch (r.head (vf glob glob 0 1))
        target (or mtch target)]
    (vf execute (.. ":edit " target))
    (when (r.nil? mtch)
      (vim.schedule md.update-file))))

(comment
  (command! :CorpusGoToShortcut (fn [] (a.println (go-to-shortcut)))))

(defn go-to-or-create-shortcut []
  "Go to the shortcut under the cursor or create one if it doesn't exist"
  (if (link?)
    (go-to-shortcut)
    (create-shortcut-on-word)))

(comment
  (command! :CorpusGoToOrCreateShortcut (fn [] (a.println (go-to-or-create-shortcut false))))
  (vnoremap "<C-]>" (fn [] (create-shortcut-on-selection)) {:buffer true :silent true})
  (nnoremap "<C-]>" (fn [] (go-to-or-create-shortcut) {:buffer true :silent true})))
