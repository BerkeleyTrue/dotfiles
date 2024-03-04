;; extends
; Aniseed queries
; (module namespace
;   {autoload {identifier namespace}}
;    require {identifier namespace}}
;    include {identifier namespace}
;    require-macros [namespace]}
;   base-export-table)
(list
  .  (symbol) @_sym (#eq? @_sym "module")
     [(multi_symbol) (symbol)] @namespace)

; defn/defn-
(list
 .  (symbol) @function.macro (#match? @function.macro "^defn\-?$") ; defn macro
 .  (symbol) @function ; defined function symbol
    (sequence (symbol)* @parameter)) ;  args

; def/def-
(list . (symbol) @function.macro (#match? @function.macro "^def\-?$"))

; defonce/defonce-
(list . (symbol) @function.macro (#match? @function.macro "defonce\-?$"))

((symbol) @keyword (#match? @keyword "^(let|alet|when-let)$")
 (sequence
   (list (symbol) @variable)
   .
   (list)))

; aniseed autoload in module def
((symbol) @keyword.import
  (#any-of? @keyword.import "autoload"))


(":" @constant (string_content) @constant)
