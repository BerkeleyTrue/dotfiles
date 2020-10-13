(module dotfiles.r
  {:require {a aniseed.core}})

(def some a.some)

(defn to-pairs [tabl]
  "(to-pairs {:a b}) => [[:a 'b']]"
  "Convert a table into an list of key,value pairs"
  (a.kv-pairs tabl))

(defn is-equal [x]
  "(is-equal 'a')"
  "creates a predicate function that will return true when called when called with arg"
  #(= x $1))

(defn find [predicate collection]
  "(find predicate collection)"
  "Iterates over elements of collection, returning the first element predicate returns truthy for."
  (a.some #(if (predicate $1) $1 false) collection))

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

(defn default-to [val x]
  "(default-to val any)"
  "returns the first value if the second value is nil"
  (if (a.nil? x) val x))

(defn tap [interceptor val]
  "(tap #(print $1) val)"
  "calls interceptor with val, then returns val"
  (interceptor x)
  x)

(defn clamp [min max val]
  "(clamp 0 1 5) => 1
  clamps val to between min and max values"
  (math.min (math.max min val) max))

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
