(module lib.async
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   import-macros [[{: acase : defasync} :lib.async-macros]]
   require-macros [macros]})

; based largely on https://github.com/ms-jpq/lua-async-await
; where we use thunks and callbacks to wrap coroutines and callback style async lua functions (i.e. (some-fn [arg1 arg2 cb] ... (cb ok some-result?)))
; functions are should always be assumed to be yield results (beware unleashing Zalgo)
; TODO: add a wrap for regular coroutines that don't yield functions

(comment
  (let [thread (coroutine.create (fn [] (assert false "Should not be seen.")))]
    (coroutine.resume thread)))

(defn unroll [func cb]
  "Unroll a coroutine until it's exhausted.
  If no callback is provided, we throw an error if the coroutine returns an error.
  func here is a coroutine, which yields functions that takes a callbacks as the last arg."
  (let [thread (coroutine.create func)
        cb (or cb (fn [ok res] (assert ok res)))] ; in the case of a call to unroll() without a callback, errors are swallowed, so we rethrow here with assert
    "Iterate recursively over thread. (NOTE: self started)"
    ((fn iter [...] ; initially empty
      ; corountine should yield a function here if not dead
      (let [(ok res) (coroutine.resume thread ...)]
        (if
          ; if coroutine is still active we assume a programmer error has occurred
          ; else, the func is has exhausted and returned not ok
          (not ok) (cb false (if (r.string? res) (.. "async.unroll: " res) res))
          ; if coroutine is dead we call the callback with the result and assume everything is fine
          (= (coroutine.status thread) :dead) (cb true res)
          ; if coroutine is still active we assume a programmer error has occurred
          (not (r.fn? res)) (cb false (.. "unroll: Expected Coroutine to yield a function but found " (type res)))
          ; coroutine is still active, we call the yielded function with the callback
          ; we call function with iter function as the callback
          (res iter)))))))

(defn thunkify* [func]
  "Wraps a function into a thunk factory function
  (thunify* func) => (factory ...args) => (thunk ...args) => res of func"
  (fn factory [& fact-args]
    (fn thunk [& thunk-args]
      (r.apply func (r.concat fact-args thunk-args)))))

(defn thunkify [func & args]
  "Wraps a function into a thunk to be consumed by await in an async context
  Meant to be used with a callback last func.
  Meant to be used in a function definition
  Func is wrapped to prevent return leakage (beware unleashing zalgo).
  Also protected-calls func to catch errors in coroutine start and
  bubbles them back out through cb.
  (thunkify func ...args) => (thunk ...args cb)"
  (let [wrapped (fn [& args]
                  (let [cb (r.last args)
                        (ok res) (pcall func ...)]
                    (when (not ok) (cb ok res))))]
    (r.apply (thunkify* wrapped) args)))

(defn schedule []
  "Schedule an async function to be executed in vim context.
  Useful for async functions that need to interact with vim.
  (await (schedule)) will yield until the next tick."
  (thunkify vim.schedule))

(defn await [thunk]
  "(await thunk)
  Await an async function within an async function. Sugar over coroutine.yield.
  Thunk should be a function that expects a callback argument."
  (coroutine.yield thunk))

(defn async [afunc]
  "(async afunc) => (fn [cb?])
   Runs a afunc in an async context.
   Returns a function that starts the async function process.
   Returned function accepts an optional callback which will bubble up errors thrown.
   If async function throws and no callback is provided the error is rethrown (see unroll).
   afunc is wrapped to be scheduled by vim.schedule to avoid Zalgo."
  (let [wrapped (fn [...]
                  (await (schedule))
                  (afunc ...))]
    (r.void ((thunkify* unroll) wrapped))))

(comment
  (let [x (fn [cb] (cb true :foo))
        thunk (thunkify x)
        async-fn (async
                   (fn []
                     (await (schedule))
                     (let [x (await thunk)]
                       (a.println :x x))))]
    (async-fn))
  (let [x (fn [])
        async-fn (async
                   (fn []
                     (await (schedule))
                     (assert false "Should be rethrown")
                     (let [(x y) (await (x))]
                       (a.println :should-never-be-seen)
                       :should-not-be-seen)))]
    (async-fn)))

(defn pure [val]
  "Yield a value in an async context
  (async (fn [] (let [x (pure :foo)] (print x)))) will print :foo"
  (await #($ val)))

(comment
  (let [async-fn
        (async
          (fn []
            (let [x (pure :foo)]
              (a.println :x x))))]
    (async-fn)))

(comment
  ((async
     (fn []
       (let [x (acase (pure :foo)
                (<- val (schedule))
                (pure nil :val)
                (catch
                  x (a.println :err x)))]
         (a.println :x x))))))

(comment
  (macrodebug
    (acase (pure :foo)
      (<- val (schedule))
      (<- val (schedule))
      (pure nil :val)
      (catch
        x (a.println :err x)
        _ (a.println :err x))))
  (macrodebug
    (defasync foo []
      "foo bar"
      (let [x (acase (pure :foo)
                (<- val (schedule))
                (pure nil :val)
                (catch
                  x (a.println :err x)))]
        (a.println :x x)))))
