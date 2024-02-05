(module lib.corpus.shortcuts
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
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
    (vf setline linenm linkified)))

(defn create-shortcut-on-selection []
  "Create a shortcut on the selected text"
  (let [start (vf getpos "'<") ; bufnm, lnum, col, off
        end (vf getpos "'>")
        linenm (vf line "v")] ;
    (when (and
            ; must be single line selection
            (= (. start 2) (. end 2))
            ; must not be visual line selection
            (not (= (. end 3)  2147483647))
            ; must not be an empty selection
            (not (= (- (- (. start 3) 1) (- (. end 3) 1)) 0)))
      (let [line (vf getline linenm)
            start (- (. start 3) 1)
            end (- (. end 3) 1)
            len (length line)
            words (vf strpart line start (+ (- end start ) 1))
            prefix (vf strpart line 0 start)
            postfix (vf strpart line (+ end 1) (- len end))
            linkified (.. prefix "[" words "]" postfix)]
        (vf setline linenm linkified)
        (vf cursor 0 (+ end 3))))))

(comment
  (vnoremap "<C-]>" create-shortcut-on-selection {:buffer true :silent true}))
