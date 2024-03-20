(module r.math
  {autoload
   {a aniseed.core
    lang r.lang}
   require {}
   import-macros []
   require-macros [macros]})

(defn add [x y & args]
  (if 
    (lang.nil? x) 0
    (lang.nil? y) x
    (lang.empty? args) (+ x y)
    (add (+ x y) (. args 1) (unpack (table.slice args 2)))))

(comment
  (add)
  (add 1)
  (add 1 2)
  (add 1 1 4))

(defn subtract [x y & args]
  (if 
    (lang.nil? x) 0
    (lang.nil? y) (- x)
    (lang.empty? args) (- x y)
    (subtract (- x y) (. args 1) (unpack (table.slice args 2)))))

(comment
  (subtract)
  (subtract 1)
  (subtract 10 5)
  (subtract 10 3 2))
