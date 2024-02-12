(module lib.corpus.complete
  {autoload
   {a aniseed.core
    r r
    md utils.module
    chooser corpus.private.chooser
    directories corpus.private.directories}
   require {}
   require-macros [macros]})

(defn complete [arglead cmdline _]
  (when-let [file (chooser.get_selected_file)]
    (let [file (chooser.get_selected_file)
          title (file:sub 1 -4) ; remove .md
          (prefix _) (cmdline:gsub "^%s*Corpus!?%s+" "")] ; remove prefix
      (when (vim.startswith title prefix) ; if title starts with prefix
        ;; If on "foo bar bazzzz"
        ;;                   ^
        ;; Must return "bazzzz", not "zzz".
        [(title:sub
           (+ (- (prefix:len) (arglead:len)) 1)
           -1)]))))
