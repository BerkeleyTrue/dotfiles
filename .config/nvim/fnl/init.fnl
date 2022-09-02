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

(when-let [packer (md.prequire :packer-modules)]
  (packer.main)
  (let [cb (md.prequire :plugins.colorbuddy)
        palette (. (md.prequire :theme.palette) :palette)
        (ok theme-fns) (pcall cb.main palette)]
    (when theme-fns
      (run-main :theme theme-fns)))
  (run-main :plugins)
  ; TODO: move into main plugin flow
  (md.prequire :plugins.fzf))
