(kwd_lit) @constant

;; def-like things
(list_lit
 .
 (sym_lit) @function.macro
 .
 (sym_lit) @function
 (#match? @function.macro "^(declare|def|definline|definterface|defmacro|defmethod|defmulti|defn|defn-|defonce|defprotocol|defstruct|deftype|ns)$"))
