(list
  .
  ((symbol) @_keyword) @scope (#any-of? @_keyword "let" "when-let" "if-let")
  .
  (array (symbol) @definition.var) (#set! "definition.var.scope" "local"))

; (list
;   .
;   (symbol) @keyword (#match? @keyword "^(fn|lambda|hashfn|λ|let)$")) @scope
;
; ; defonce/defonce-
; (list . (symbol) @function.macro (#match? @function.macro "defonce\-?$") . (symbol) @definition.var)
;
; ; def/def-
(list
  .
  (symbol) @_callee (#any-of? @_callee "def" "def-" "defonce")
  .
  (symbol) @definition.var
  (#set! "definition.var.scope" "parent")); defined function symbol
;
; ; defn/defn-
(list
  . (symbol) @_callee (#any-of? @_callee "fn" "defn" "defn-") ; defn macro
    (symbol) @definition.function
    (#set! "definition.function.scope" "parent"); defined function symbol
    (array (symbol)? @definition.parameter)) @scope ;  args
;
