(module r.collections
  {autoload
   {a aniseed.core
    lang r.lang}
   require {}
   import-macros []
   require-macros [macros]})

(defn conj [col x & xs]
  (if 
    (lang.nil? col) []
    (lang.nil? x) col
    (lang.empty? xs) (a.concat col [x])
    (a.concat col [x] xs)))

(comment
  (conj)
  (conj [1 2 3])
  (conj [1 2 3] 4)
  (conj [1 2 3] 4 5 6))

(defn cons [x xs]
  (var res [x])

  (each [_ x (ipairs xs)]
    (table.insert res x))

  res)

(comment
  (cons 1 [])
  (cons 4 [1 2 3]))

(defn reverse [xs]
  (a.reduce (fn [xs x] (cons x xs)) [] xs))

(comment
  (reverse [])
  (reverse [1 2 3 4 5]))
