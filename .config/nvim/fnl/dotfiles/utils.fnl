(module dotfiles.utils
  {:require {nvim aniseed.nvim
             a aniseed.core}})

(defn viml->lua [m f opts]
  "(viml->lua :module.a :module-function {:args ['foo' 'bar']})"
  (..
    "lua require('" m "')"
    "['" f "']"
    "(" (or (and opts opts.args) "") ")"))

(defn nnoremap [lhs rhs options]
  "(nnoremap 'cr' ':echo foo' {:expr true :buffer false :nowait false})
  create a nnoremap"
  (let [{:expr expr
         :silent silent
         :buffer buffer
         :nowait nowait
         :script script
         :unique unique} (or options {})

        args ["n" lhs rhs {:expr expr
                           :silent silent
                           :nowait nowait
                           :script script
                           :noremap true}]]

    (if
      buffer (nvim.buf_set_keymap 0 (unpack args))
      (nvim.set_keymap (unpack args)))))

(defn get-char-under-curs []
  "(get-char-under-curs)
  get the character under the cursor
  should work with multi-byte chars but is slower than other methods"
  (let [line (nvim.fn.getline ".")
        col (nvim.fn.col ".")
        matchReg (.. "\\%" col "c.")]
    (nvim.fn.matchstr line matchReg)))
