(module init
  {require-macros [macros]})

(let [(ok res) (pcall require :packer-modules)]
  (if
    ok (do
         ((. (require :theme) :main))
         (require :plugins.nerd-commenter)
         (require :plugins.scroll-fix)
         (require :plugins.sexp)
         (require :plugins.defx)
         (require :plugins.fzf)
         (require :plugins.accents)
         (require :plugins.markdown)
         (require :plugins.corpus)
         (require :plugins.tree-sitter)
         (require :plugins.runtime-utils)
         (require :plugins.indent-guides)
         (require :plugins.telescope))
     (print "could not load packer modules")))
