(module dotfiles.utils
  {:require {nvim aniseed.nvim
             a aniseed.core}})

(defn viml->lua [m f opts]
  (..
    "lua require('" m "')"
    "['" f "']"
    "(" (or (and opts opts.args) "") ")"))

(defn nnoremap [lhs rhs is-buffer is-expr]
  (when is-buffer
    (nvim.buf_set_keymap 0 "n" lhs rhs {:silent true :expr (if (a.nil? is-expr) false is-expr) :noremap true})
    (nvim.set_keymap "n" lhs rhs {:silent true :expr (if (a.nil? is-expr) false is-expr) :noremap true})))
