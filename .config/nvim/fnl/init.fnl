(module init
  {:require
   {md utils.module}
   :require-macros [macros]})

; vim options requires no plugins
; side-effecty
(require :options)
(require :maps)
(require :functions)

(when (md.prequire :packer-modules)
  (do
    (run-main :theme)
    (run-main :slackline)
    (run-main :plugins)
    (md.prequire :plugins.scroll-fix)
    (md.prequire :plugins.fzf)
    (md.prequire :plugins.accents)
    (md.prequire :plugins.runtime-utils)))
