(module r.collections
  {autoload
   {a aniseed.core
    lang r.lang}
   require {}
   import-macros []
   require-macros [macros]})

(defn to-pairs [tabl]
  "(to-pairs {:a b}) => [[:a 'b']]"
  "Convert a table into an list of key,value pairs"
  (a.kv-pairs tabl))

(defn from-pairs [prs]
  "(from-pairs [...[key val]]) => Dict[key val]"
  (->>
    prs
    (a.reduce
      (fn [acc [key val]]
        (tset acc key val)
        acc)
      {})))

(defn size [collection]
  (if 
    (lang.table? collection)
    ; first try maxn, which works on sequential tables
    (let [cursize (table.maxn collection)]
      (if (> cursize 0)
        cursize
        ; zero could mean an empty seq or a table with key/vals
        ; turn into key/val pairs, then grab size
        (table.maxn (to-pairs collection))))

    (lang.string? collection) 
    (length collection)

    0))


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

(defn some [f xs]
  (var result false)
  (var n* 1)
  (var count (a.count xs))
  (while (and (not result) 
              (<= n* count))
    (when-let [candidate (f (. xs n*))]
      (set result candidate))
    (set n* (a.inc n*)))
  result)

(comment
  (some #(> $ 30) [34 35 37 40])
  (some #(< $ 30) [34 35 37 40])

  (some #(< $ 30) [34 35 27 40])
  (some #(> $ 30) [34 35 27 40]))

(defn every [f xs]
  (var result true)
  (var n* 1)
  (var count (a.count xs))
  (when (= 0 count) 
    (set result false))
  (while (and result 
              (<= n* count))
    (set result (and result (f (. xs n*))))
    (set n* (a.inc n*)))
  result)

(comment
  (every #(> $ 30) [34 35 37 40])
  (every #(> $ 30) [34 35 27 40]))

(defn sort-by [f xs]
  (var out [])
  (each [_ val (pairs xs)]
    (table.insert out val))
  (table.sort out f)
  out)

(comment
  (sort-by #(> $1 $2) [:a :b :z :c])
  (sort-by #(< $1 $2) [:a :b :z :c]))

(defn group-by [f xs]
  (a.reduce
    (fn [acc x]
      (let [key (f x)]
        (if (not (. acc key))
          (tset acc key []))
        (table.insert (. acc key) x)
        acc))
    {}
    xs))

(comment
  (group-by #(if (> $ 3) :bigger :smaller) [1 2 3 4 5 6]))
