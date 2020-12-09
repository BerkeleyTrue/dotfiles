(module plugins.sandwich
  {:require {r r
             utils utils}})

(def- recipes
  [
   ; default
   {:buns ["\\s\\+" "\\s\\+"] :regex 1 :kind [:delete :replace :query] :input [" "]}
   {:buns ["" ""] :action [:add] :motionwise [:line] :linewise 1 :input ["\\<CR>"]}
   {:buns ["^$" "^$"] :regex 1 :linewise 1 :input ["\\<CR>"]}
   {:buns ["<" ">"] :expand_range 0}
   {:buns ["\"" "\""] :quoteescape 1 :expand_range 0 :nesting 0 :linewise 0}
   {:buns ["\"" "\""] :quoteescape 1 :expand_range 0 :nesting 0 :linewise 0}
   {:buns ["`" "`"] :quoteescape 1 :expand_range 0 :nesting 0 :linewise 0}
   {:buns ["{" "}"] :nesting 1 :skip_break 1}
   {:buns ["[" "]"] :nesting 1}
   {:buns ["(" ")"] :nesting 1}

   {:buns "sandwich#magicchar#t#tag()" :listexpr 1 :kind [:add] :action [:add] :input [:t :T]}
   {:buns "sandwich#magicchar#t#tag()" :listexpr 1 :kind [:replace] :action [:add] :input [:T]}
   {:buns "sandwich#magicchar#t#tagname()" :listexpr 1 :kind [:replace] :action [:add] :input [:t]}

   {:external ["\\<Plug>(textobj-sandwich-tag-i)" "\\<Plug>(textobj-sandwich-tag-a)"] :noremap  0 :kind  [:delete :textobj] :expr_filter ["operator#sandwich#kind() !=# 'replace'"] :synchro 1 :linewise 1 :input [:t :T]}
   {:external ["\\<Plug>(textobj-sandwich-tag-i)" "\\<Plug>(textobj-sandwich-tag-a)"] :noremap  0 :kind  [:replace :query] :expr_filter ["operator#sandwich#kind() ==# 'replace'"] :synchro 1 :input [:T]}
   {:external ["\\<Plug>(textobj-sandwich-tagname-i)" "\\<Plug>(textobj-sandwich-tagname-a)"] :noremap  0 :kind  [:replace :textobj] :expr_filter ["operator#sandwich#kind() ==# 'replace'"] :synchro 1 :input [:t]}

   {:buns ["sandwich#magicchar#f#fname()" ")"] :kind [:add :replace] :action [:add] :expr 1 :input [:f]}

   {:external ["\\<Plug>(textobj-sandwich-function-ip)" "\\<Plug>(textobj-sandwich-function-i)"] :noremap 0 :kind [:delete :replace :query] :input [:f]}
   {:external ["\\<Plug>(textobj-sandwich-function-ap)" "\\<Plug>(textobj-sandwich-function-a)"] :noremap 0 :kind [:delete :replace :query] :input [:F]}

   {:buns "sandwich#magicchar#i#input('operator')" :kind [:add :replace] :action [:add] :listexpr 1 :input [:i]}
   {:buns "sandwich#magicchar#i#input('textobj' 1)" :kind [:delete :replace :query] :listexpr 1 :regex 1 :synchro 1 :input [:i]}
   {:buns "sandwich#magicchar#i#lastinput('operator' 1)" :kind [:add :replace] :action [:add] :listexpr 1 :input [:I]}
   {:buns "sandwich#magicchar#i#lastinput('textobj')" :kind [:delete :replace :query] :listexpr 1 :regex 1 :synchro 1 :input [:I]}

   ;; spaces
   {:buns ["{ " " }"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["{"]}
   {:buns ["[ " " ]"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["["]}
   {:buns ["( " " )"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["("]}
   {:buns ["( " " )"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["("]}
   {:buns ["{\\s*" "\\s*}"]     :nesting 1 :regex 1 :match_syntax 1 :kind [:delete :replace :textobj] :action [:delete] :input ["{"]}
   {:buns ["\\[\\s*" "\\s*\\]"] :nesting 1 :regex 1 :match_syntax 1 :kind [:delete :replace :textobj] :action [:delete] :input ["["]}
   {:buns ["(\\s*" "\\s*)"]     :nesting 1 :regex 1 :match_syntax 1 :kind [:delete :replace :textobj] :action [:delete] :input ["("]}])

(defn main []
  (tset utils.g :sandwich#recipes recipes))

(comment (. utils.g :sandwich#recipes))
