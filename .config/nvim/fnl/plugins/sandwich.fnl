(module plugins.sandwich
  {:require {r r
             utils utils}})

(def- additions
  [{:buns ["{ " " }"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["{"]}
   {:buns ["[ " " ]"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["["]}
   {:buns ["( " " )"] :nesting 1 :match_syntax 1 :kind [:add :replace] :action [:add] :input ["("]}
   {:buns ["{\\s*" "\\s*}"]   :nesting 1 :regex 1 :match_syntax 1 :kind [:delete :replace :textobj] :action [:delete] :input ["{"]}
   {:buns ["\\[\\s*" "\\s*\\]"] :nesting 1 :regex 1 :match_syntax 1 :kind [:delete :replace :textobj] :action [:delete] :input ["["]}
   {:buns ["(\\s*" "\\s*)"]   :nesting 1 :regex 1 :match_syntax 1 :kind [:delete :replace :textobj] :action [:delete] :input ["("]}])

(defn main []
  (let [default_recipes (. utils.g :sandwich#default_recipes)
        recipes (r.concat default_recipes additions)]
    (tset utils.g :sandwich#recipes recipes)))
