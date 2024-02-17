(module lib.async
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})


(defn unroll [func cb]
  "Unroll a coroutine until it's exhausted."
  (let [thread (coroutine.create func)
        cb (or cb #nil)]
    "Iterate recursively over thread. (NOTE: self started)"
    ((fn iter [...]
      (let [(stat ret) (coroutine.resume thread ...)]
        (assert stat ret) ; programmer error
        (if (= (coroutine.status thread) :dead)
          (cb ret)
          (do
            (assert (r.fn? ret) "Expected coroutine to yield a function.")
            (ret iter))))))))

(defn thunkify [func]
  "Wraps a function into a thunk factory function"
  (fn factory [& fact-args]
    (fn thunk [& thunk-args]
      (r.apply func (r.concat fact-args thunk-args)))))

(def- async* (thunkify unroll))

(defn async [func]
  "Unwrap an async function"
  (async* func))

(defn await [afunc]
  "Await a async function within an async function. Sugar over coroutine.yield."
  (coroutine.yield afunc))

(comment
  (((thunkify (fn [x y cb] (cb x y))) 1 2 ) #(a.println :args $...))
  (unroll
    (fn [...]
      (a.println :args ...)
      (let [x (coroutine.yield ((thunkify (fn [cb] (cb 1)))))]
        (a.println :x x)
        (let [(y z) (coroutine.yield ((thunkify (fn [cb] (cb 2 3)))))]
          (a.println :y y :z z))))))
