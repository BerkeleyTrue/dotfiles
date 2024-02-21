(module init
  {require
   {a aniseed.core
    md utils.module}
   require-macros [macros]})

; vim options requires no plugins
; side-effecty
(require :options)
(require :maps)

(run-main :plugins)
(run-main :lib.scroll-fix)
(run-main :lib.folds)
(run-main :lib.corpus)
(run-main :lib.accents)
(run-main :lib.treesitter.queries)
(let [cb (md.prequire :plugins.colorbuddy)
      palette (. (md.prequire :theme.palette) :palette)
      (ok theme-fns) (pcall cb.main palette)]
  (when theme-fns
    (run-main :theme theme-fns)))
