(module utils
  {:require {nvim aniseed.nvim
             a aniseed.core
             str aniseed.string}})


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

(defn get-cursor-pos []
  "(get-cursor-pos) => [x, y]
  get the chars under the cursor"
  [(nvim.fn.line ".")
   (nvim.fn.col ".")])

(defn get-char-under-curs []
  "(get-char-under-curs)
  get the character under the cursor
  should work with multi-byte chars but is slower than other methods"
  (let [line (nvim.fn.getline ".")
        col (nvim.fn.col ".")
        matchReg (.. "\\%" col "c.")]
    (nvim.fn.matchstr line matchReg)))

(def- none "NONE")

(defn hi-link [from to override?]
  "create a highlight link"
  (if override? (nvim.ex.highlight_ :link from to)
      (nvim.ex.highlight :link from to)))

(defn hi-link! [from to]
  "create a highlight! link"
  (hi-link from to true))

(defn highlight [scope fg bg attrs special]
  "create a highlight using a term/gui palette"
  (let [bg (or bg (none))

        attrs (str.join
                ", "
                (a.filter
                  #(= (type $1) "string")
                  (or attrs [none])))

        special (or special [none none])

        fg (if (and (not= (a.first special) none)
                    (= (a.first fg) none)
                    (not (nvim.fn.has "gui_running")))
             special
             fg)

        guifg (.. "guifg='" (a.first fg) "'")
        ctermfg (.. "ctermfg='" (a.second fg) "'")

        guibg (.. "guibg='" (a.first bg) "'")
        ctermbg (.. "ctermbg='" (a.second bg) "'")

        gui (.. "gui='" attrs "'")
        cterm (.. "cterm='" attrs "'")

        guisp (.. "guisp='" (a.first special) "'")]

    (nvim.ex.highlight scope gui cterm guifg ctermfg guibg ctermbg guisp)))
