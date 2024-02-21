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
  (<- a body)
  (pure a body)
  (catch
    apattern body
    pattern body
    pattern body))

; example
(acase (<- (afunc1) ; async function call, returning a boolean and optionally a result
  (<- a (test1)) ; the first return of afunc1 is true, the result is set to the value a
  (pure x 'foobar') ; use pure to return a value
  (catch
    failure (print failure) ; if afunc or test1 return false, the results will be set to apattern, here as failure
    ; normal pattern matching from here
    _ (print e)))

<- maps to await in the async module, pure maps to pure, which wraps a value in a callback
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
  (each [_ [f pattern body & rest] (ipairs body)]
    (when (not= (tostring f) :catch) ; should always be last
      (case last-body
        :<-
        (let [pattern (if
                        (table? pattern)
                        `(true ,(unpack pattern))

                        (= (tostring pattern) :nil)
                        true

                        `(true ,pattern))]

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
      (let [pattern (if
                      (table? pattern)
                      `(false ,(unpack pattern))

                      (= (tostring pattern) :nil)
                      false

                      `(false ,pattern))]

        (table.insert new-body `(catch ,pattern ,body ,(unpack rest))))

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
         (asm#.async (fn [] ,(unpack body)))))))

:return M
