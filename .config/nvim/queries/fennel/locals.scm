(list
  .
  (symbol) @keyword (#match? @keyword "^(fn|lambda|hashfn|λ|let)$")) @scope

; defonce/defonce-
(list . (symbol) @function.macro (#match? @function.macro "defonce\-?$") . (symbol) @definition.var)

; def/def-
(list . (symbol) @function.macro (#match? @function.macro "^def\-?$") . (symbol) @definition.var)

; defn/defn-
(list
 .  (symbol) @function.macro (#match? @function.macro "^defn\-?$") ; defn macro
    (symbol) @definition.function ; defined function symbol
    (array (symbol)* @definition.parameter)) @scope ;  args

