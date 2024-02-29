(module r.lang
  {autoload
   {a aniseed.core
    r r
    hask r.curry}
   require {}
   require-macros [macros]})

(def is-equal (hask.curry #(= $1 $2)))

(defn number? [val] (= (type val) :number))
(comment
  (number? 0)
  (number? :foo))

(defn boolean? [val] (= (type val) :boolean))
(defn true? [val] (= val true))
(defn false? [val] (= val false))
(defn fn? [f] (= (type f) :function))
(defn string? [val] (= (type val) :string))
(defn nil? [val] (= (type val) :nil))
(defn exists? [val] (not= val nil))
(defn table? [val] (= (type val) :table))
; array|string|table
(defn empty? [val]
  "(empty? {}) ;=> true
   (empty? []) ;=> true
   (empty? \"\") ;=> true"
  (if
    (table? val) (nil? (next val))
    (string? val) (= (length val) 0)
    (nil? val) true
    false))

(comment
  (empty? {})
  (empty? [])
  (empty? "")
  (empty? {:foo :bar})
  (empty? [:foo])
  (empty? "foo")
  (empty? nil))

(defn not-empty? [val] (not (empty? val)))
