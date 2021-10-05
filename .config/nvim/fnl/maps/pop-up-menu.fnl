(module maps.pop-up-menu
  {:require {: r
             : utils}
   :require-macros [macros]})

(defn expand-on-enter []
  (let [expand-return (if (pcall utils.ex.packadd :delimitMate)
                        utils.fn.delimitMate#ExpandReturn
                        #"\\<CR>")
        expand-snip (if (pcall utils.ex.packadd :ultisnips)
                      utils.fn.UltiSnips#ExpandSnippet
                      #"\\<CR>")
        ; try to expand snip
        snippet (expand-snip)]

    (utils.fn.expand
      (if
        ; if ulti says we can snip, then return
        (> (. utils.g :ulti_expand_res) 0) snippet
        ; if PUM is visible go down
        ; text expansion does not work through fennel/lua->viml
        (> (utils.fn.pumvisible) 0) " "
        ; let delimitMate take over if able
        (expand-return)))))

; On enter, check for snippet and expand
; or return <CR>
comment (utils.inoremap
         :<CR>
         (..
           ; <C-R>{reg} will insert what ever is in register you choose
           ; using = as the reg indicates you want to insert the result of an expression
           ; we give it a lua exp that will run one of these functions
           ; then the result of that exp is inserted.
           "<C-R>="
           (utils.viml->luaexp *module-name* (sym->name expand-on-enter))
           "<CR>")
         {:silent true})

; On tab with dropdown go down
; else insert tab
(comment (utils.inoremap
          :<TAB>
            "pumvisible() ? \"\\<C-n>\" : \"\\<TAB>\" "
          {:silent true
           :expr true}))


(comment (utils.inoremap
          :<S-TAB>
          "pumvisible() ? \"\\<C-p>\" : \"\\<S-TAB>\" "
          {:silent true
           :expr true}))
