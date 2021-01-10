(boolean) @boolean
(nil) @constant.builtin
(string) @string
(number) @number
(keyword) @constant
(comment) @comment
(symbol) @identifier

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"]
@punctuation.bracket

; functions
(list . (symbol) @function)
(list . (keyword) @function)

((symbol) @include (#match? @include "^(require|require-macros|import\\-macros|include)$"))

((symbol) @repeat (#match? @repeat "^(each|for|while)$"))

((symbol) @conditional (#match? @conditional "^(if|when)$"))
((symbol) @function (#match? @function "^#$"))

((symbol) @keyword (#match? @keyword "^(fn|lambda|hashfn|set|tset|λ|global|var|local|let|do|not|not=|_ENV|_G|_VERSION|arg|assert|collectgarbage|comment|coroutine|debug|dofile|doto|error|eval\\-compiler|gensym|getmetatable|in\\-scope?|ipairs|list|list?|load|loadfile|loadstring|match|macro|macrodebug|macroexpand|macros|multi\\-sym?|next|pairs|package|pcall|print|rawequal|rawget|rawlen|rawset|select|sequence?|setmetatable|string|sym|sym?|table|table?|tonumber|tostring|type|unpack|varg?|xpcall)$"))

; Aniseed queries
; (module
;    namespace
;    {require {identifier namespace}}
;     include {identifier namespace}
;     require-macros [namespace]}
;    base-export-table)
(list
  .  (symbol) @function.macro (#eq? @function.macro "module")
     (symbol) @namespace)

; defn/defn-
(list
 .  (symbol) @function.macro (#match? @function.macro "^defn\-?$") ; defn macro
    (symbol) @function ; defined function symbol
    (array (symbol)* @parameter)) ;  args

; def/def-
(list . (symbol) @function.macro (#match? @function.macro "^def\-?$"))

; defonce/defonce-
(list . (symbol) @function.macro (#match? @function.macro "defonce\-?$"))
