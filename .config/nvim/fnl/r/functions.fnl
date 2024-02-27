(module r.functions
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

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

(defn rearg [func indexes]
  "Creates a function that invokes func with arguments arranged according to the specified indexes.
  (rearg(f, [2, 1, 0]) is equivalent to (f args[2] args[1] args[0])."
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

(def const a.constantly)
(def noop (const nil))
(defn call [f]
  (assert (= (type f) :function) (.. "expected f to be a function but found: " (tostring f)))
  (f))

(def apply
  (curry
    (fn [f args] (f (unpack args)))))

(defn over [...]
  "Creates a function that invokes each provided function with the
  arguments the resulting function receives and returns the results.
  ((over max min) [1 2 3 4 5]) => [5 1]"
  (let [fs [...]]
    (fn [...]
      (let [args [...]]
        (a.map
          (fn [f] (apply f args))
          fs)))))

(defn negate [f]
  "Creates a function that negates the result of the predicate func."
  (fn negator [...] (not (f ...))))

(defn void [f]
  "Creates a function invokes f with the provided arguments and returns nil."
  (fn voider [...]
    (f ...)
    nil))

(defn defer-throttle [f]
  "Creates a throttled function that delays invoking f until after vim.schedule has been called
  Calls to a throttled function while we are waiting are ignored.
  f is called with the arguments of the last call to the throttled function."
  (var running? false)
  (var args nil)
  (fn debounced [...]
    (set args [...])
    (when-not running?
      (set running? true)
      (vim.schedule
        (fn []
          (set running? false)
          (apply f args))))))


