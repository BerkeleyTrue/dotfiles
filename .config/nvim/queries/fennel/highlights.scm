(boolean) @boolean
(nil) @constant.builtin
(string) @string
(number) @number
(field) @constant
(comment) @comment
(identifier) @variable

[
 "fn"
 "lambda"
 "hashfn"
 "set"
 "tset"
 "λ"
 "global"
 "var"
 "local"
 "let"
 "do"
 "not"
 "not="
 "_ENV"
 "_G"
 "_VERSION"
 "arg"
 "assert"
 "collectgarbage"
 "coroutine"
 "debug"
 "dofile"
 "doto"
 "error"
 "eval-compiler"
 "gensym"
 "getmetatable"
 "in-scope?"
 "ipairs"
 "list"
 "list?"
 "load"
 "loadfile"
 "loadstring"
 "match"
 "macro"
 "macrodebug"
 "macroexpand"
 "macros"
 "multi-sym?"
 "next"
 "pairs"
 "package"
 "pcall"
 "print"
 "rawequal"
 "rawget"
 "rawlen"
 "rawset"
 "select"
 "sequence?"
 "setmetatable"
 "string"
 "sym"
 "sym?"
 "table"
 "table?"
 "tonumber"
 "tostring"
 "type"
 "unpack"
 "varg?"
 "xpcall"]
@keyword

[
 "require"
 "require-macros"
 "import-macros"
 "include"]
@include

[
  "each"
  "for"
  "while"]
@repeat

[
  "if"
  "when"]
@conditional

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"]
@punctuation.bracket

; hash function
"#" @function

(function_definition
  name: (identifier) @function)

(lambda_definition
  name: (identifier) @function)

; function call with single identifier
; need to combine with below
(function_call
  name: (identifier) @function .)

; function call with object getter
(function_call
    name: (field_expression
            (identifier) @variable
            "."
            (identifier) @function .))

; property getter
(field_expression
   (identifier)
   "." @punctuation.delimiter
   (identifier)* @property)

; no idea what this targets
(field_expression
   (identifier)
   (field) @function)

(parameters (identifier) @parameter)

; methods
; TODO needs : delimiter to work
; ((function_call
;     name: ([(identifier) @variable
;             (field_expression (identifier)*)])
;     (field) @method))

; Aniseed queries
; (module namespace {require {identifier namespace}
;                    include {identifier namespace}
;                    require-macros [namespace]})
(function_call
   name: (identifier) @function.macro (#eq? @function.macro "module")
   [(identifier) @namespace
    (field_expression
      (identifier) @namespace
      ("." (identifier) @namespace)*)] @namespace
   (table
     ([(field) (identifier)]? @include (#match? @include "^:?require(-macros)?$|^:?include$"))?)? @aniseed-imports)


; def/def-
((function_call
    name: (identifier) @function.macro (#match? @function.macro "^def\-?$")))

; defn/defn-
((function_call
    name: (identifier) @function.macro (#match? @function.macro "^defn\-?$")
    (identifier) @function
    (sequential_table (identifier)* @parameter)))

; defonce/defonce-
((function_call
  name: (identifier) @function.macro (#match? @function.macro "^defonce\-?$")))
