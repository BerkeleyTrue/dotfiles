(module r.functions
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    hask r.curry
    lang r.lang}
   require {}
   require-macros [macros]})

(defn rearg [func indexes]
  "Creates a function that invokes func with arguments arranged according to the specified indexes.
  (rearg(f, [2, 1, 0]) is equivalent to (f args[2] args[1] args[0])."
  (assert (not (a.some (lang.is-equal 0) indexes)) "Lua/fennel is 1 indexed")
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
  (hask.curry
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

(defn- set-timeout [ms cb]
  (let [timer (vim.loop.new_timer)]
    (vim.loop.timer_start
      timer
      ms
      0
      (vim.schedule_wrap
        (fn []
          (timer:stop)
          (timer:close)
          (cb))))
    timer))

; see: https://github.com/lodash/lodash/blob/master/debounce.js
(defn debounce [wait f opts]
  "Creates a debounced function that delays invoking f until after ms milliseconds have passed.
  Can be invoked with the :leading? option to cause f to be invoked on the leading edge of the timeout."
  (assert (r.fn? f) (.. "expected f to be a function but found " (type f)))
  (let [{: leading?} (or opts {})]

    (var results nil)
    (var timer nil)
    (var invoking? false)
    (var last-call-time 0)
    (var last-args nil)

    (fn invoke []
      "Invokes f with the last arguments and sets the result."
      (let [args last-args]
        (set last-args nil)
        (let [res [(r.apply f args)]]
          (set results res)
          res)))

    (fn remaining-wait [tme]
      "Returns the remaining wait time."
      (let [time-since-last-call (- tme last-call-time)]
        (- wait time-since-last-call)))

    (fn should-invoke? [tme]
      "Returns true if the debounced function should invoke f."
      (let [time-since-last-call (- tme last-call-time)]
        (or
          (= last-call-time 0) ; first call
          (>= time-since-last-call wait) ; activity has stopped
          (> 0 time-since-last-call)))) ; we went back to the future

    (fn trailing-edge []
      (set timer nil)
      (when last-args
        (invoke))
      (set last-args nil)
      results)

    (fn timer-expired []
      (let [now (os.time)]
        (if (should-invoke? now)
          (trailing-edge)
          (set timer (set-timeout (remaining-wait now) timer-expired)))))

    (fn leading-edge [tme]
      (set timer (set-timeout wait timer-expired))
      (if leading?
        (invoke)
        results))

    (fn cancel []
      "Cancels the debounced function."
      (when timer
        (timer:stop)
        (timer:close)))

    (fn flush []
      "Invokes the wrapped function and returns the result."
      (if-not timer
        results
        (trailing-edge)))

    (fn debounced [& args]
      "The debounced function."
      (let [tme (os.time)
            invoking? (should-invoke? tme)]
        (set last-args args)

        (if (and invoking? (= timer nil))
          (leading-edge tme)
          (do
            (set timer (set-timeout wait timer-expired))
            results))))


    (setmetatable
      {:cancel cancel
       :flush flush
       :f debounced}
      {:__call debounced})))


