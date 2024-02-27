(module r
  {autoload
   {a aniseed.core
    str aniseed.string}
   require
   {datetime r.datetime
    _strings r.strings
    fns r.functions}
   require-macros [macros]})

; hack to re-export imports
(a.merge! *module* datetime)
(a.merge! *module* fns)
(a.merge! *module* _strings)


;; ### tables - data first
(def get a.get)
; Returns the value at the 'key in the 'table, or 'default if the key is not present."
; ["table" "key" "default"]

(def get-in a.get)
; "Returns the value in the path 'keys in the 'table, or 'default if the key is not present."
; ["table" "keys" "default"])

(def assoc a.assoc)
(def assoc-in a.assoc-in)
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

(def update a.update)
(def keys a.keys)
(def vals a.vals)

(defn iter->list [iter]
  "Convert an iterator into a table"
  (icollect [val iter] val))

;; ### array - data last
(def forEach a.run!)
(def for-each a.run!)

(defn map [f xs]
  (a.map f xs))

(defn map-indexed [f xs]
  (a.map-indexed f xs))

(def filter
  (fns.curry
    (fn [predicate arr]
      (assert (= (type predicate) :function) (.. "Expected a function as the first argument but found " (tostring predicate)))
      (assert (= (type arr) :table) (.. "Expected a seq as the second argument but found " (tostring arr)))
      (a.filter predicate arr))))

(def reject
  (fns.curry
    (fn [func arr]
      (filter #(not (func $...)) arr))))

; TODO: should return k/v for table
(defn head [val]
  "get the first element of a collection"
  (if
    (= (type val) :string) (: val :sub 1 1)
    (a.first val)))

(comment
  (head "foo" )
  (head [1 2 3])
  (head {:a 1 :b 2}))

(defn tail [val]
  "get the rest of a collection"
  (if (= (type val) :string) (: val :sub 2)
    (a.rest val)))

(comment
  (tail "foo")
  (tail [1 2 3])
  (tail {:a 1 :b 2}))

(defn last [val]
  "Get the last element of a collection"
  (if (= (type val) :string) (: val :sub -1)
    (a.last val)))

(comment
  (last "foo")
  (last [1 2 3])
  (last {:a 1 :b 2}))

(def some a.some)
(def merge a.merge)
(def concat a.concat)
(def initial a.butlast)
(defn conj [coll & items]
  "Conj adds items to the end of a collection"
  (a.concat coll items))

(defn flatten [xs]
  (a.reduce
    (fn [res item]
      (if (= (type item) :table)
        (concat res item)
        (do
          (table.insert res item)
          res)))
    []
    xs))

(defn flatMap [f xs]
  (->>
    xs
    (a.map f)
    (flatten)))

(def reduce a.reduce)
(defn join [separater xs]
  "Join a list of strings with a separater"
  (if
    (= (length xs) 1) (a.first xs)
    (a.reduce
      (fn [acc item] (.. acc separater item))
      (head xs)
      (tail xs))))

(comment (join ", " ["a" "list"]))

;; ### array
(defn find [predicate collection]
  "(find predicate collection)"
  "Iterates over elements of collection, returning the first element predicate returns truthy for."
  (a.some #(if (predicate $1) $1 false) collection))

(defn find-index [predicate collection]
  "(find-index predicate collection)"
  "Iterates over elements of collection, returning the index element predicate returns truthy for."
  (var found false)
  (var idx 1)
  (let [cnt (a.count collection)]
    (while (and (not found) (<= idx cnt))
      (let [res (predicate (. collection idx))]
        (when res
          (set found true))
        (when (not found)
          (set idx (a.inc idx)))))
    (if found idx -1)))

;; ### utils
(defn default-to [val x]
  "(default-to val any)"
  "returns the first value if the second value is nil"
  (if (a.nil? x) val x))

;; ### seq
(defn tap [interceptor val]
  "(tap #(print $1) val)"
  "calls interceptor with val, then returns val"
  (interceptor val)
  val)


;; ### numbers
(defn clamp [min max val]
  "(clamp 0 1 5) => 1
  clamps val to between min and max values"
  (math.min (math.max min val) max))

(defn round [num decimals]
  "(round 1.234 2) => 1.23
  rounds a number to a specified number of decimal places"
  (let [mult (math.pow 10 decimals)]
    (/ (math.floor (* num mult)) mult)))

;; ### array
(defn slice [start end array]
  "(slice 0 3 [1 2 3 4])
  Creates a slice of `array` from `start` up to, but not including, `end`."
  (let [result []
        len (length array)
        start (if
               (= start 0) 1
               (< start 0) (if
                             (>= (- start) len) 1
                             (+ len start))
               (> start len) len
               start)
        end (if
              (> end len) len
              (if
                (<= end 0) (+ len end)
                end))]

    (for [i start (- end 1) 1]
      (table.insert result (. array i)))
    result))

;; ### array
(defn chunk [size collection]
  "(chunk 2 [1 2 3 4]) => [[1 2] [3 4]]
  Creates an array of elements split into groups the length of size.
  If array can't be split evenly, the final chunk will be the remaining elements."
  (let [len (length collection)
        result []]

    (when (and (> len 0) (> size 1))
      (var idx 1)
      (while (< idx len)
        (table.insert result (slice idx (+ idx size) collection))
        (set idx (+ idx size))))
    result))

;; ### utils
(def range
  (fns.curry
    (fn [start end]
      (let [result []]
        (for [i start end 1]
          (table.insert result i))
        result))))

(comment
  (range 4)
  (range 1 5))

;; ### lang
(def is-equal (fns.curry #(= $0 $2)))
(defn number? [val] (= (type val) :number))
(comment (number? 0) (number? :foo))
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

(defn key-map [keys]
  "create a key-map from a list of keys."
  (->>
    keys
    (a.map #[$1 $1])
    (from-pairs)))

(defn uniq [list]
  "create a dublicate-free version of a list."
  (->
    list
    (key-map)
    (keys)))

(defn size [collection]
  (if (table? collection)
    ; first try maxn, which works on sequential tables
    (let [cursize (table.maxn collection)]
      (if (> cursize 0)
        ; zero could mean an empty seq or a table with key/vals
        cursize
        ; turn into key/val pairs, then grab size
        (table.maxn (to-pairs collection))))
    (string? collection) (length collection)
    0))

(defn includes? [col val]
  "(includes? [:foo :bar] :foo) ;=> true
   (includes? [:foo :bar] :baz) ;=> false
   (includes? \"foo\" :foo) ;=> true
   (includes? \"foo\" :bar) ;=> false"
  (if
    (string? col) (not (nil? (: col :find val)))
    (table? col) (not (nil? (find #(= $1 val) col)))
    false))

(defn re-includes? [val pattern]
  "Will perform a regex search for pattern in val, returning true if found"
  (not= (vim.fn.match val pattern) -1))

(defn slice [...]
  "Create a slice of an seq table, from start up to and including end
  (slice 2 4 [1 2 3 4 5]) => [3 4]
  (slice 2 [1 2 3 4 5]) => [3 4 5]
  (slice [1 2 3 4 5]) => [1 2 3 4 5]"
  (let [args [...]
        nargs (length args)
        arr (last args)
        end (if (> nargs 2)
              (. args 2)
              (size arr))
        start (if (> nargs 1)
                (. args 1)
                1)]
    (var out [])
    (for [i start end 1]
      (table.insert out (. arr i)))
    out))

(comment
  (slice 2 4 [1 2 3 4 5])
  (slice 2 [1 2 3 4 5])
  (slice [1 2 3 4 5]))

(defn drop [_n seq]
  "Drop n number of elements from a sequential table"
  (let [_n (or _n 1)]
    (slice (+ _n 1) seq)))

(comment (drop 2 [1 2 3 4 5]))

(defn take [_n seq]
  "Take n number of elements from a sequential table"
  (let [_n (or _n 1)]
    (slice 1 (+ _n 1) seq)))

(comment (take 2 [1 2 3 4 5]))
