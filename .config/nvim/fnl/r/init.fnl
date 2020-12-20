(module r
  {:require {a aniseed.core
             str aniseed.string}
   :require-macros [macros]})



;utils
(def _ :placeholder)
(defn curry [func arity]
  "Creates a function that accepts arguments of func and either invokes func returning its result,
  if at least arity number of arguments have been provided, or returns a function that
  accepts the remaining func arguments, and so on. The arity of func may be specified."
  (assert
    (= (type func) "function")
    (.. "curry expected a function but received: " (tostring func)))
  (assert
    (if arity (= (type arity) "number") true)
    (.. "curry expected an optional arity of type number but received: " (tostring arity)))
  (let [arity (or arity (. (debug.getinfo func "u") :nparams))]
    (if
      (< arity 1) func
      (do
        (defn wrapper [args needed]
          (if
            (< needed 1) (func (unpack args))
            (fn [...]
              (let [args (a.concat args [...])
                    needed (- needed (select :# ...))]
                (wrapper args needed)))))
        (wrapper [] arity)))))

(def is-equal (curry #(= $1 $2)))

(defn rearg [func indexes]
  (assert (not (a.some (is-equal 0) indexes)) "Lua/fennel is 1 indexed")
  (fn [...]
    (let
      [args [...]
       newargs (a.reduce
                 (fn [newargs idx]
                   (table.insert newargs (a.get args idx))
                   newargs)
                 []
                 indexes)]
      (func (unpack newargs)))))

;; funcs
(def const a.constantly)
(def noop (const nil))
(defn call [f]
  (assert (= (type f) :function) (.. "expected f to be a function but found: " (tostring f)))
  (f))

(def apply
  (curry
    (fn [args f] (f (unpack args)))))


;; tables - data first
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

;; array - data last
(def forEach a.run!)
(def for-each a.run!)
(def map a.map)
(def filter a.filter)
(def head a.first)
(def merge a.merge)
(def tail a.rest)
(def last a.last)
(def some a.some)
(def concat a.concat)
(def reduce a.reduce)
(def join
  (curry
    (fn [separater xs]
      (if
        (= (length xs) 1)) (a.first xs)
        (a.reduce (fn [acc item] (.. acc separater item)) (head xs) (tail xs)))))

;; array
(defn find [predicate collection]
  "(find predicate collection)"
  "Iterates over elements of collection, returning the first element predicate returns truthy for."
  (a.some #(if (predicate $1) $1 false) collection))

;; array
(defn find-index [predicate collection]
  "(find-index predicate collection)"
  "Iterates over elements of collection, returning the index element predicate returns truthy for."
  (var found false)
  (var n 1)
  (let [cnt (a.count collection)]
    (while (and (not found) (<= n cnt))
      (let [res (predicate (. collection n))]
        (when res
          (set found true))
        (when (not found)
          (set n (a.inc n)))))
    (if found n -1)))
;; utils
(defn default-to [val x]
  "(default-to val any)"
  "returns the first value if the second value is nil"
  (if (a.nil? x) val x))

;; seq
(defn tap [interceptor val]
  "(tap #(print $1) val)"
  "calls interceptor with val, then returns val"
  (interceptor val)
  val)


;; numbers
(defn clamp [min max val]
  "(clamp 0 1 5) => 1
  clamps val to between min and max values"
  (math.min (math.max min val) max))

;; array
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

;; array
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

;; utils
(def range
  (curry
    (fn [start end]
      (let [result []]
        (for [i start end 1]
          (table.insert result i))
        result))))

;; Strings
(def split (curry str.split))
(defn upperFirst [s]
  (let [first (-> s (: :sub 1 1) (: :upper))
        rest (s:sub 2)]
    (.. first rest)))

(defn words [str]
  (let [str (tostring str)
        words []]
    (each [word (str:gmatch "%S+")]
      (table.insert words word))
    words))

(defn- create-compounder [cb]
  (fn [str]
    (->>
      str
      (words)
      (a.reduce cb ""))))

(def pascal-case
  (create-compounder
    (fn [acc word]
      (->>
        word
        (string.lower)
        (upperFirst)
        (.. acc)))))


;; lang
(defn true? [a] (= a true))
(defn false? [a] (= a false))
(defn function? [f] (= (type f) :function))
(defn string? [val] (= (type val) :string))
(defn nil? [val] (= (type val) :nil))
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

(defn key-map [keys]
  (->>
    keys
    (a.map #[$1 $1])
    (from-pairs)))
