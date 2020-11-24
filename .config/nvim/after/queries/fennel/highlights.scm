((function_call
    name: (identifier) @function.macro (#eq? @function.macro "module")
    [(identifier)
     (field_expression (identifier)+)] @namespace))

((function_call
    name: (identifier) @function.macro (#match? @function.macro "^def\-?$")))

((function_call
    name: (identifier) @function.macro (#match? @function.macro "^defn\-?$")
    (identifier) @function
    (sequential_table (identifier)+ @parameter)))

((function_call
  name: (identifier) @function.macro (#match? @function.macro "^defonce\-?$")))
