(module rx.observer
  {:require {a aniseed.core
             r r}})


;  @class Observer
;  @description Observers are simple objects that receive values from Observables.
(def- Observer {})

(a.assoc Observer :__index Observer)
(a.assoc Observer :__toString (r.const "Observer"))

; (create onNext onError onComplete)
;  Creates a new Observer.
;  @arg {function=} onNext - Called when the Observable produces a value.
;  @arg {function=} onError - Called when the Observable terminates due to an error.
;  @arg {function=} onCompleted - Called when the Observable completes normally.
;  @returns {Observer}
(defn create [onNext onError onCompleted]
  (let [self {
              :_onNext  (or onNext r.noop)
              :_onError (or onError error)
              :_onCompleted  (or onCompleted r.noop)
              :stopped false}]

    (setmetatable self Observer)))

;  Pushes zero or more values to the Observer.
;  @arg {*...} values
(tset Observer :onNext
      (fn [self ...]
        (when (not self.stopped)
          ; TODO: should alway dispatch one arg
          (self._onNext (values ...)))))

;  Notify the Observer that an error has occurred.
;  @arg {string=} message - A string describing what went wrong.
(tset Observer :onError
      (fn [self message]
        (when (not self.stopped)
          (set self.stopped true)
          (self._onError message))))

;  Notify the Observer that the sequence has completed and will produce no more values.
(tset Observer :onCompleted
      (fn [self]
        (when (not self.stopped)
          (set self.stopped true)
          (self._onCompleted))))
