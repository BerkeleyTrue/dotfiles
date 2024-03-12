(module init
  {autoload
   {a aniseed.core
    options options
    plugins plugins

    lisp-indent lib.treesitter.lisp-indent
    scroll-fix lib.scroll-fix
    folds lib.folds
    corpus lib.corpus
    accents lib.accents
    ts-queries lib.treesitter.queries
    sexps lib.sexps
    whitespace lib.whitespace
    git lib.git
    yadm lib.yadm
    glow lib.glow
    marks lib.marks

    cb plugins.colorbuddy

    pl theme.palette
    theme theme}
   
   require {}
   require-macros [macros]})

(defn main []
  ; vim options requires no plugins
  ; side-effecty
  (options.main)
  (require :maps)
  (plugins.main)
  (scroll-fix.main)
  (folds.main)
  (corpus.main)
  (accents.main)
  (ts-queries.main)
  (sexps.main)
  (whitespace.main)
  (git.main)
  (yadm.main)
  (glow.main)
  (lisp-indent.main)
  (marks.main)

  (when-let [(ok theme-fns) (pcall cb.main (. pl :palette))]
    (theme.main theme-fns)))

(main)
