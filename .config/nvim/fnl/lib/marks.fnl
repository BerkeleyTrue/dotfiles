(module lib.marks
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(comment
  (vf getmarklist) ; global
  (vf getmarklist "%")) ; buffer local

(def- sign-cache 
  (r.reduce
    #(r.assoc $1 $2 true)
    {}
    [:. :^ "`" "'" "\"" :< :> "[" "]" 
     0 1 2 3 4 5 6 7 8 9]))

(def- buf-cache {})

(defn create-sign [text linenr])

(defn delete-sign [])

(defn register-mark [mark linenr col bufnr])
