(module init
  {require
   {a aniseed.core
    md utils.module}
   require-macros [macros]})

; vim options requires no plugins
; side-effecty
(require :options)
(require :maps)
(require :functions)

(run-main :plugins)
(run-main :lib.scroll-fix)
(let [cb (md.prequire :plugins.colorbuddy)
      palette (. (md.prequire :theme.palette) :palette)
      (ok theme-fns) (pcall cb.main palette)]
  (when theme-fns
    (run-main :theme theme-fns)))
