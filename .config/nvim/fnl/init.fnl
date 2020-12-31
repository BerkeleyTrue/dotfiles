(module init
  {:require-macros [macros]})

; vim options requires no plugins
; side-effecty
(require :options)
(require :maps)
(require :functions)

(let [(ok res) (pcall require :packer-modules)]
  (if
    ok (do
         (run-main :theme)
         (run-main :slackline)
         (run-main :plugins)
         (safe-require :plugins.scroll-fix)
         (safe-require :plugins.fzf)
         (safe-require :plugins.accents)
         (safe-require :plugins.runtime-utils))
    (print "could not load packer modules")))
