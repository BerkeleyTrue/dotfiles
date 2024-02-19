(list
  .
  ((symbol) @_keyword) @scope (#any-of? @_keyword "let" "when-let" "if-let")
  .
  (array (symbol) @local.definition.var))

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
  (symbol) @local.definition.var
  (#set! definition.var.scope "parent")); defined function symbol
;
; ; defn/defn-
(list
  . (symbol) @_callee (#any-of? @_callee "defn" "defn-") ; defn macro
    (symbol) @local.definition.function (#set! definition.function.scope "parent"); defined function symbol
    (array (symbol)? @local.definition.parameter)) @local.scope ;  args

; fn
(list
  . (symbol) @_callee (#any-of? @_callee "fn") ; defn macro
    (symbol) @local.definition.function (#set! definition.function.scope "parent"); defined function symbol
    (array (symbol)? @local.definition.parameter)) @local.scope ;  args
