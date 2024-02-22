(module lib.async-test.fnl
  {autoload
   {{: unroll
     : thunkify*
     : thunkify
     : async
     : await
     : schedule} lib.async}
   require-macros [macros]})

(deftest unroll-1
  (let [th1 (fn [x]
              (fn [cb]
               (t.= x :x "expected 1 arg to be x")
               (t.= :function (type cb))
               (cb 2)))
        th2 (fn [y z]
              (fn [cb]
                (t.= y :y "expected first arg to be y")
                (t.= z :z "expected second arg to be z")
                (t.= :function (type cb))
                (cb 2 3)))
        (ok res) (unroll
                   (fn []
                     (let [x (coroutine.yield (th1 :x))]
                       (t.= x 2)
                       (let [(y z) (coroutine.yield (th2 :y :z))]
                         (t.= y 2)
                         (t.= z 3)
                         :done)))
                   (fn [& args]
                     (t.= 2 (length args))
                     (t.= :done (. args 2))))]
    (t.ok? ok res)
    (t.= nil res)))

(deftest unroll-throwing
  (let [(ok res) (unroll
                   (fn []
                     (assert false "Should be seen by callback"))
                   (fn [ok err]
                     (t.ok? (not ok))
                     (t.= :string (type err))
                     (t.ok? (string.find err "unroll"))))]
    (t.ok? ok res))

  (let [(ok err) (pcall unroll (fn [] (assert false "Should be thrown")))]
    (t.= false ok)
    (t.= :string (type err))
    (t.ok? (string.find err "unroll")))

  (let [(ok err) (pcall unroll (fn [] (coroutine.yield :foo)))]
    (t.= false ok)
    (t.= :string (type err))
    (t.ok? (string.find err "unroll"))))

(deftest thunkify*
  (let [afn (fn [x y cb]
              (t.= 1 x)
              (t.= 2 y)
              (cb x y)
              ; this is generally not desired in userland
              ; but internal lib need to return for coroutines to work correctly
              :bad)
        thunk-factory (thunkify* afn)
        thunk (thunk-factory 1 2)
        res (thunk (fn [x y & args]
                     (t.= 1 x)
                     (t.= 2 y)
                     (t.= 0 (length args))))]
    (t.= :bad res)))

(deftest thunkify
  (let [afn (fn [x y cb]
              (t.= 1 x)
              (t.= 2 y)
              (cb x y)
              :should-not-be-seen)
        thunk (thunkify afn 1 2)
        res (thunk (fn [x y & args]
                     (t.= 1 x)
                     (t.= 2 y)
                     (t.= 0 (length args))))]
    (t.= nil res))

  (let [afn (fn [x y]
              (t.= 1 x)
              (t.= 2 y)
              (assert false "Should be consumed by callback"))
        thunk (thunkify afn 1 2)]
    (thunk (fn [ok res]
             (t.ok? (not ok))
             (t.= :string (type res))
             (t.ok? (string.find res "Should be consumed by callback"))))))

(deftest async
  (let [x (fn [y cb]
            (t.= :bar y)
            (vim.wait 200)
            (cb :foo y)
            :baz)
        thunk-x (thunkify x :bar)
        async-fn (async
                   (fn []
                     (let [(x y) (coroutine.yield thunk-x)]
                       (t.= :foo x)
                       (t.= :bar y)
                       :done)))
        res (async-fn)]
    (t.= nil res))

  (let [x (fn [] (assert false "Should be consumed in async func"))
        async-fn (async
                   (fn []
                     (await (thunkify vim.schedule))
                     (let [(x y) (await (thunkify x :bar))]
                       (t.ok? (not ok))
                       (t.= :string (type y))
                       (t.ok? (string.find y "Should be consumed in async func"))
                       :done)))
        res (async-fn)]
    (t.= nil res))

  ; these next two tests are going to throw errors
  ; since we force async to allways run yield internally
  ; we can't catch this error w/o a callback
  ; but test will still pass
  (let [x (fn [] (assert false "Should be rethrown"))
        async-fn (async
                   (fn []
                     (let [(x y) (await (x))]
                       (print :should-never-be-seen)
                       :should-not-be-seen)))
        (ok res) (pcall async-fn)]
    (t.ok? ok "zalgo! async function should not be pcall-able")
    (t.= nil res))

  (let [x (fn [])
        async-fn (async
                   (fn []
                     (assert false "Should be rethrown")
                     (let [(x y) (await (x))]
                       (print :should-never-be-seen)
                       :should-not-be-seen)))
        (ok res) (pcall async-fn)]
    (t.ok? ok "zalgo! async function should not be pcall-able")
    (t.= nil res))

  (let [x (fn [])
        async-fn (async
                   (fn []
                     (assert false "Should be rethrown")
                     (let [(x y) (await (x))]
                       (print :should-never-be-seen)
                       :should-not-be-seen)))]
    (async-fn (fn [ok res]
                (t.ok? (not ok))
                (t.= :string (type res))
                (t.ok? (string.find res "Should be rethrown")))))
  (do
    (var called false)
    (let [x (fn [cb]
              (t.= :function (type cb) "timer called without callback")
              (let [timer (vim.loop.new_timer)]
                (vim.loop.timer_start timer 100 0 (fn [] (cb :foo)))))

          async-fn (async
                     (fn []
                       (let [(x y) (await (thunkify x))]
                         (set called true)
                         (t.ok? x "timer failed")
                         (t.= nil y))))]
      (async-fn)
      (vim.wait 200)
      (t.= true called "x was not called")))

  (do
    (var called false)
    (let [x (fn [cb]
              (t.= :function (type cb) "timer called without callback")
              (let [timer (vim.loop.new_timer)]
                (vim.loop.timer_start timer 100 0 (fn [] (cb :foo)))))

          async-fn (async
                     (fn []
                       (let [(x y) (await (thunkify x))]
                         (set called true)
                         (t.ok? x "timer failed")
                         (t.= nil y))))]
      (async-fn (fn [ok res]
                  (t.ok? ok "async function failed")
                  (t.= true called "x was not called"))))))

(deftest schedule
  (do
    (var called 0)
    (let [call #(set called (+ called 1))
          afn (async
                (fn []
                  (call)
                  ; (print :called0)
                  (await (schedule))
                  (assert false "Should be rethrown")
                  (call)))]
      (afn (fn [ok err]
             (t.= 1 called)
             (call)
             ; (print :called1)
             (t.= false ok)
             (t.= :string (type err))
             (t.ok? (string.find err "Should be rethrown"))))
      (vim.wait 200)
      (t.= 2 called "schedule was not called"))))

  ; (do
  ;   (var called 0)
  ;   (let [call #(set called (+ called 1))
  ;         afn (async
  ;               (fn []
  ;                 (call)
  ;                 (await (schedule))
  ;                 (print :called2)
  ;                 (assert false "Should be rethrown")
  ;                 (call)))]
  ;     (afn)
  ;     (vim.wait 200)
  ;     (t.= 1 called "async method was not called"))))
