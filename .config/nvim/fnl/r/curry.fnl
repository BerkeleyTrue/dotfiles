(module r.curry
  {autoload
   {a aniseed.core}
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
