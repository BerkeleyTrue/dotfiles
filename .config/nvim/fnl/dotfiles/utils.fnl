(module dotfiles.utils
  {:require {nvim aniseed.nvim
             a aniseed.core}})

(defn viml->lua [m f opts]
  (..
    "lua require('" m "')"
    "['" f "']"
    "(" (or (and opts opts.args) "") ")"))

(defn nnoremap [lhs rhs options]
  ; create a nnoremap
  ; usage (nnoremap "cr" ":echo 'foo'" {:expr true :buffer false :nowait false}))
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


    (if buffer
      (nvim.buf_set_keymap 0 (unpack args))

      (nvim.set_keymap (unpack args)))))

(defn get-char-under-curs [offset]
  ; (get-char-under-curs offset)
  ; get the character under the cursor offset by offset num of chars
  ; might not work with multi-byte chars
  (let [line (nvim.fn.getline ".")
        col (-> "."
                (nvim.fn.col)
                (- 1)
                (+ offset))]

    (-> line
        (nvim.fn.strpart col)
        (nvim.fn.strcharpart 0 1))))
