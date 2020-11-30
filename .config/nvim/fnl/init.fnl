(module init
  {:require-macros [macros]})

(let [(ok res) (pcall require :packer-modules)]
  (if
    ok (do
         (run-main :theme)
         (run-main :plugins.init)
         (safe-require :plugins.nerd-commenter)
         (safe-require :plugins.scroll-fix)
         (safe-require :plugins.sexp)
         (safe-require :plugins.defx)
         (safe-require :plugins.fzf)
         (safe-require :plugins.accents)
         (safe-require :plugins.markdown)
         (safe-require :plugins.corpus)
         (safe-require :plugins.tree-sitter)
         (safe-require :plugins.runtime-utils)
         (safe-require :plugins.indent-guides)
         (safe-require :plugins.telescope))
    (print "could not load packer modules")))
