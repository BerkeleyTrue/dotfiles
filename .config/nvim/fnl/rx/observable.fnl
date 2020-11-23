(module rx.observable
  {:require {Observer rx.observer
             a aniseed.core
             r r}})


;  @class Observable
;  @description Observables push values to Observers.
(def- Observable {})

(tset Observable :__index Observable)
(tset Observable :__toString (r.const "Observable"))


; Creates a new Observable.
;  @arg {function} subscribe - The subscription function that produces values.
;  @returns {Observable}
(defn create [subscribe]
  (let [self {:_subscribe subscribe}]
    (setmetatable self Observable)))

;  Shorthand for creating an Observer and passing it to this Observable's subscription function.
;  @arg {function} onNext - Called when the Observable produces a value.
;  @arg {function} onError - Called when the Observable terminates due to an error.
;  @arg {function} onCompleted - Called when the Observable completes normally.
(tset Observable :subscribe
      (fn [self onNext onError onCompleted]
        (if
          (= (type onNext) "table") (self._subscribe onNext)
          (self._subscribe (create onNext onError onCompleted)))))

; Returns an Observable that immediately completes without producing a value.
(defn empty []
  (create (fn [observer]
            (observer:onCompleted))))

; Returns an Observable that never produces values and never completes.
(defn never []
  (create (fn [observer])))

;  Returns an Observable that immediately produces an error.
(defn throw [message]
  (create (fn [observer] (observer:onError message))))

;  Creates an Observable that produces a set of values.
;  @arg {*...} values
;  @returns {Observable}
(defn of [...]
  (let [vals [...]]
    (create (fn [observer]
              (r.forEach #(observer:onNext $1) vals)
              (observer:onCompleted)))))

;  Creates an Observable that produces a range of values from `start` up to `end`
;  @arg {number} start - The first value of the range, or the upper limit if no other arguments
;                          are specified.
;  @arg {number=} end - The max value of the range.
;  @returns {Observable}
(defn fromRange [start end]
  (let [range (r.range start end)]
    (of (unpack range))))

;  Creates an Observable that produces values from a table.
;  @arg {table} table - The table used to create the Observable.
;  @returns {Observable}
(defn fromArray [arr]
  (of (unpack arr)))
;

;  Creates an Observable that produces values when the specified coroutine yields.
;  @arg {thread|function} fn - A coroutine or function to use to generate values.  Note that if a
;                              coroutine is used, the values it yields will be shared by all
;                              subscribed Observers (influenced by the Scheduler), whereas a new
;                              coroutine will be created for each Observer when a function is used.
;  @returns {Observable}
; function Observable.fromCoroutine(fn, scheduler)
;   return Observable.create(function(observer)
;                            local thread = type(fn) == 'function' and coroutine.create(fn) or fn
;                            return scheduler:schedule(function()
;                                                      while not observer.stopped do
;                                                      local success, value = coroutine.resume(thread)

;                                                      if success then
;                                                      observer:onNext(value)
;                                                      else
;                                                      return observer:onError(value)
;                                                      end

;                                                      if coroutine.status(thread) == 'dead' then
;                                                      return observer:onCompleted()
;                                                      end

;                                                      coroutine.yield()
;                                                      end
;                                                      end)
;                            end)
; end

;  Creates an Observable that produces values from a file, line by line.
;  @arg {string} filename - The name of the file used to create the Observable
;  @returns {Observable}
; function Observable.fromFileByLine(filename)
;   return Observable.create(function(observer)
;                            local file = io.open(filename, 'r')
;                            if file then
;                            file:close()

;                            for line in io.lines(filename) do
;                            observer:onNext(line)
;                            end

;                            return observer:onCompleted()
;                            else
;                            return observer:onError(filename)
;                            end
;                            end)
; end

;  Creates an Observable that creates a new Observable for each observer using a factory function.
;  @arg {function} factory - A function that returns an Observable.
;  @returns {Observable}
(defn defer [factory]
  (if
    (not (or factory (= (type factory) "function"))) (error (a.pr "Expected a function but received: ")factory)
    (setmetatable
      {:subscribe (fn [_ ...]
                    (let [observable (factory)]
                       (observable:subscribe (values ...))))}
      Observable)))



;  Returns an Observable that repeats a value a specified number of times.
;  @arg {*} value - The value to repeat.
;  @arg {number=} count - The number of times to repeat the value.  If left unspecified, the value
;                         is repeated an infinite number of times.
;  @returns {Observable}
(defn replicate [value count]
   (create (fn [observer]
             (var cnt count)
             (while (or (= cnt nil) (> cnt 0))
               (observer:onNext value)
               (if count (set cnt (- cnt 1)))
               (observer:onCompleted)))))



;  Subscribes to this Observable and prints values it produces.
;  @arg {string=} name - Prefixes the printed messages with a name.
;  @arg {function=tostring} formatter - A function that formats one or more values to be printed.
; (tset Observable :dump (name, formatter)
;   name = name and (name .. ' ') or ''
;   formatter = formatter or tostring

;   local onNext = function(...) print(name .. 'onNext: ' .. formatter(...)) end
;   local onError = function(e) print(name .. 'onError: ' .. e) end
;   local onCompleted = function() print(name .. 'onCompleted') end

;   return self:subscribe(onNext, onError, onCompleted))
; end
