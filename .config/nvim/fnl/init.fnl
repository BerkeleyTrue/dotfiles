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
  (when-let [cb (md.prequire :plugins.colorbuddy)]
    ; TODO: idempodize theme
    (run-main :theme (cb.main)))
  (run-main :slackline)
  (run-main :plugins)
  ; TODO: move into main plugin flow
  (md.prequire :plugins.scroll-fix)
  (md.prequire :plugins.fzf)
  (md.prequire :plugins.accents)
  (md.prequire :plugins.runtime-utils))
