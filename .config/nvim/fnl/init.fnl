(module init
  {:require-macros [macros]})

; vim options requires no plugins
(run-main :options)

(let [(ok res) (pcall require :packer-modules)]
  (if
    ok (do
         (run-main :theme)
         (run-main :plugins)
         (safe-require :plugins.scroll-fix)
         (safe-require :plugins.defx)
         (safe-require :plugins.fzf)
         (safe-require :plugins.accents)
         (safe-require :plugins.runtime-utils))
    (print "could not load packer modules")))
