(module lib.async
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(comment
  (let [thread (coroutine.create (fn [] (assert false "Should not be seen.")))]
    (coroutine.resume thread)))

(defn unroll [func cb]
  "Unroll a coroutine until it's exhausted."
  (let [thread (coroutine.create func)
        cb (or cb #nil)]
    "Iterate recursively over thread. (NOTE: self started)"
    ((fn iter [...]
      (let [(ok res) (coroutine.resume thread ...)]
        (assert ok (when (r.string? res) (.. "unroll: " res)))
        (if (= (coroutine.status thread) :dead)
          (cb res)
          (do
            (assert (r.fn? res) "Expected coroutine to yield a function.")
            (res iter))))))))

(comment
  (let [th1 (fn [] (fn [cb] (cb 1)))
        th2 (fn [] (fn [cb] (cb 2 3)))]
    (unroll (fn []
              (a.println :start)
              (let [x (coroutine.yield (th1))]
                (a.println :x x)
                (let [(y z) (coroutine.yield (th2))]
                  (a.println :y y :z z)
                  :done)))))
  (unroll (fn [] (assert false "Should not be seen."))))

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
  "Wraps a function into a thunk to be consumed by await in an async context"
  (r.apply (thunkify* (r.void func)) args))

(comment
  (let [afn (fn [x y cb] (cb x y)
              :should-not-be-seen)
        thunk (thunkify afn 1 2)]
    (thunk #(a.println :args $...))))

(defn async [func]
  "Runs a function in an async context."
  ((thunkify* unroll) func))

(defn await [afunc]
  "Await a async function within an async function. Sugar over coroutine.yield."
  (coroutine.yield afunc))

(comment
  (let [x (fn [y cb] (vim.wait 1000) (cb :foo y) :foo)
        thunk-x (thunkify x :bar)
        async-fn (async
                   (fn []
                     (let [(x y) (await thunk-x)]
                       (a.println :x x :y y)
                       :done)))]
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
    (async-fn)))
