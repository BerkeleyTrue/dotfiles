(module rx.subscription
  {:require {r r}})

; @class Subscription
; @description A handle representing the link between an Observer and an Observable, as well as any
; work required to clean up after the Observable completes or the Observer unsubscribes.
(def- Subscription  {})
(tset Subscription :__index Subscription)
(tset Subscription :__tostring (r.const "Subscription"))

; --- Creates a new Subscription.
; -- @arg {function=} action - The action to run when the subscription is unsubscribed. It will only
; --                           be run once.
; -- @returns {Subscription}
; function Subscription.create(action)
;   local self = {
;                 action = action or util.noop,
;                 unsubscribed = false}


;   return setmetatable(self, Subscription)
; end

; --- Unsubscribes the subscription, performing any necessary cleanup work.
; function Subscription:unsubscribe()
;   if self.unsubscribed then return end
;   self.action(self)
;   self.unsubscribed = true
; end

; return Subscription
