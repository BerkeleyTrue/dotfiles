(module lib.async
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   import-macros [[{: acase} :lib.async-macros]]
   require-macros [macros]})

(comment
  (let [thread (coroutine.create (fn [] (assert false "Should not be seen.")))]
    (coroutine.resume thread)))

(defn unroll [func cb]
  "Unroll a coroutine until it's exhausted.
  If no callback is provided, we throw an error if the coroutine returns an error."
  (let [thread (coroutine.create func)
        cb (or cb (fn [ok res] (assert ok res)))] ; in the case of a call to unroll() without a callback, errors are swallowed, so we rethrow here with assert
    "Iterate recursively over thread. (NOTE: self started)"
    ((fn iter [...]
      (let [(ok res) (coroutine.resume thread ...)]
        (if
          (not ok) (cb ok (if (r.string? res) (.. "async.unroll: " res) res))
          (= (coroutine.status thread) :dead) (cb true res)
          (not (r.fn? res)) (cb false (.. "Expected Coroutine to yield a function but found " (type res)))
          (res iter)))))))

(comment
  (let [th1 (fn [] (fn [cb] (cb 1)))
        th2 (fn [] (fn [cb] (cb 2 3)))]
    (unroll (fn []
              (a.println :start)
              (let [x (coroutine.yield (th1))]
                (a.println :x x)
                (let [(y z) (coroutine.yield (th2))]
                  (a.println :y y :z z)
                  :done)))
            (fn [...] (a.println :res ...))))
  (unroll (fn [] (assert false "Should be seen by callback")) (fn [...] (a.println :cb ...)))
  (unroll (fn [] (assert false "Should be thrown"))))

(defn- thunkify* [func]
  "Wraps a function into a thunk factory function"
  (fn factory [& fact-args]
    (fn thunk [& thunk-args]
      (r.apply func (r.concat fact-args thunk-args)))))

(comment
  (let [afn (fn [x y cb]
              (cb x y)
              ; this is generally not desired in userland
              ; but internal lib need to return for coroutines to work correctly
              :bad)
        thunk-factory (thunkify* afn)
        thunk (thunk-factory 1 2)]
    (thunk #(a.println :args $...))))

(defn thunkify [func & args]
  "Wraps a function into a thunk to be consumed by await in an async context
  Meant to be used with a callback last function.
  Function is wrapped to prevent return leakage.
  Also protected calls function to catch errors."
  (let [wrapped (fn [& args]
                  (let [cb (r.last args)
                        (ok res) (pcall func ...)]
                    (when (not ok) (cb ok res))))]
    (r.apply (thunkify* wrapped) args)))

(comment
  (let [afn (fn [x y cb] (cb x y)
              :should-not-be-seen)
        thunk (thunkify afn 1 2)]
    (thunk #(a.println :args $...)))

  (let [afn (fn [] (assert false "Should be consumed by callback"))
        thunk (thunkify afn 1 2)]
    (thunk #(a.println :args $...))))

(defn async [afunc]
  "(async afunc) => (fn [cb?])
   Runs a function in an async context.
   Returns a function that starts the async function process.
   If async function throws and no callback is provided the error is rethrown."
  (r.void ((thunkify* unroll) afunc)))

(defn await [thunk]
  "(await thunk)
  Await an async function within an async function. Sugar over coroutine.yield."
  (coroutine.yield thunk))

(comment
  (let [x (fn [y cb] (vim.wait 1000) (cb :foo y) :foo)
        thunk-x (thunkify x :bar)
        async-fn (async
                   (fn []
                     (let [(x y) (await thunk-x)]
                       (a.println :x x :y y)
                       :done)))]
    (async-fn))

  (let [x (fn [] (assert false "Should be consumed in async func"))
        async-fn (async
                   (fn []
                     (await (thunkify vim.schedule))
                     (let [(x y) (await (thunkify x :bar))]
                       (a.println :x x :y y)
                       :done)))]
    (async-fn))


  (let [x (fn [] (assert false "Should be rethrown"))
        async-fn (async
                   (fn []
                     (let [(x y) (await (x))]
                       (a.println :should-never-be-seen)
                       :should-not-be-seen)))]
    (async-fn))

  (let [x (fn [])
        async-fn (async
                   (fn []
                     (assert false "Should be rethrown")
                     (let [(x y) (await (x))]
                       (a.println :should-never-be-seen)
                       :should-not-be-seen)))]
    (async-fn)))

(defn schedule []
  "Schedule an async function to be executed in vim context.
  Useful for async functions that need to interact with vim."
  (thunkify vim.schedule))

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
  "Yield a value in an async context"
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
