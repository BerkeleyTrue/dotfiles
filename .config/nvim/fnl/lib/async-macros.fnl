(local module-sym (gensym))

(local M {})

; =<< module system >=>
(macro defn [name args ...]
  "defines an exported function 'name'."
  `(tset
    M
    ,(tostring name)
    (fn ,name ,args ,...)))


; =<< async >=>
(defn acase [expr & body]
  "Perform chained async function calls, any of which may fail.
(acase (<- expr)
  (<- a (test))
  (catch
    e (print e)))
  "
  (local syms {:<- (gensym :<-)
               :pure (gensym :pure)})
  (var new-expr nil)
  (var last-body nil)

  (match expr
    (where [f & body] (= (tostring f) :<-))
    (do
      (set new-expr `(,syms.<- ,(unpack body)))
      (set last-body :<-))

    (where [f & body] (= (tostring f) :pure))
    (do
      (set new-expr `(,syms.pure ,(unpack body)))
      (set last-body :pure))

    _ (assert-compile false (.. "Invalid async expression: " (view expr))))

  (var new-body (list))
  (each [_ [f pattern body] (ipairs body)]
    (when (not= (tostring f) :catch) ; should always be last
      (case last-body
        :<-
        (let [pattern (if
                        (table? pattern)
                        `(true ,(unpack pattern))

                        (= (type pattern) :nil)
                        :true

                        pattern)]

          (table.insert new-body pattern))

        :pure
        (table.insert new-body pattern)

        _ (assert-compile false (.. "Invalid async pattern type: " last-body))))

    (case (tostring f)
      :<-
      (do
        (table.insert new-body `(,syms.<- ,body))
        (set last-body :<-))

      :pure
      (do
        (table.insert new-body `(,syms.pure ,body))
        (set last-body :pure))

      ; should always be last
      :catch
      (table.insert new-body `(catch ,pattern ,body))

      _ (assert-compile false (.. "Invalid async body: " (view f)))))

  `(let [asm# (require :lib.async)
         {:await ,syms.<-
          :pure ,syms.pure} asm#]
     (case-try ,new-expr
       ,(unpack new-body))))

(defn defasync [name args doc? & body]
  "define an async function"
  (let [body (if (= (type doc?) :string) body `(,doc? ,(unpack body)))]
    `(defn ,name ,args
       ,(if (= (type doc?) :string) doc? "")
       (let [asm# (require :lib.async)]
         ((asm#.async (fn [] ,(unpack body))))))))

:return M
