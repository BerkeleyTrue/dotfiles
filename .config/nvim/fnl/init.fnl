(module init
  {require
   {md utils.module}
   require-macros [macros]})

; vim options requires no plugins
; side-effecty
(require :options)
(require :maps)
(require :functions)

(when-let [packer (md.prequire :packer-modules)]
  (packer.main)
  (when-let [cb (md.prequire :plugins.colorbuddy)]
    (let [palette (. (md.prequire :theme.palette) :palette)]
      (run-main :theme (cb.main palette))))
  (run-main :plugins)
  ; TODO: move into main plugin flow
  (md.prequire :plugins.scroll-fix)
  (md.prequire :plugins.fzf)
  (md.prequire :plugins.accents)
  (md.prequire :plugins.runtime-utils))
