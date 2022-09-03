(local module-sym (gensym))

; TODO: move out of macro
(fn run-main [name ...]
  `(let [args# ,[...]]
     ; conditional threadfirst
     (-?>
       ,name

       ; pcall require
       ((fn [name#]
          (let [(ok# res#) (pcall require name#)]
            (if
              ok# res#
              (print (.. "Could not load module "
                         (tostring name#)
                         ": " (tostring res#)))))))
       ((fn [mod#]
          (assert
            (= (type mod#) "table")
            (.. "run-main expects a table to be exported by " (tostring ,name) " but found: " (tostring mod#)))
          (assert
            (= (type (. mod# :main)) "function")
            (.. "run-main expects a module with a public main function but found " (tostring (. mod# :main)) " for " (tostring ,name)))
          mod#))

       ((fn [mod#] ((. mod# :main) (unpack args#)))))))

(fn sym->name [a-sym]
  "Symbol to name as a string."
  (tostring a-sym))

(fn from-iter [iter-fact]
  "(from-iter (pairs {:apple \"red\" :orange \"orange\"})) ;=> {:red \"apple\" :orange \"orange\"}
   When vals are
   (from-iter (: \"foo bar\" :gmatch \"%S+\")) ;=> {:foo true :bar true}"
  `(let [tbl# {}]
     (each [k# v# ,iter-fact]
       (tset tbl# k# (if (= v# nil) true v#)))
     tbl#))

(fn from-seq-iter [iter-fact]
  "(from-seq-iter (ipairs [:apple :orange])) ;=> [:apple :orange]"
  `(let [tbl# []]
     (each [_# v# ,iter-fact]
       (table.insert tbl# v#))
     tbl#))

(fn when-let
  [bindings ...]
  (assert
     (= (type bindings) "table")
     (.. "expects a table for its binding but found : " (tostring bindings)))
  (assert
    (= 2 (table.maxn bindings)) "exactly 2 forms in binding vector")
  (let [form (. bindings 1)
        tst (. bindings 2)]
    `(let [temp# ,tst]
       (when temp#
         (let [,form temp#]
           ,...)))))

(fn when-not
  [test ...]
  (list (sym :if)
        (list (sym :not) test)
        (list (sym :do) ...)))

(fn if-let
  [bindings then else]
  (assert
     (= (type bindings) "table")
     (.. "expects a table for its binding but found : " (tostring bindings)))
  (assert
    (= 2 (table.maxn bindings)) "exactly 2 forms in binding vector")
  (let [form (. bindings 1)
        tst (. bindings 2)]
    `(let [temp# ,tst]
       (if temp#
         (let [,form temp#]
           ,then)
         ,else))))

(fn logx [x]
  "(logx foo) ;=> (print \"foo: \" foo)"
  `(print ,(.. (tostring x) ": ") ,x))


(fn viml->lua* [symb opts]
  `(..
     "lua require('" *module-name* "')"
     "['" ,(tostring symb) "']"
     "(" ,(or (and opts opts.args) "") ")"))

(fn cviml->lua* [symb opts]
  `(.. "<CMD>" (viml->lua* ,symb ,opts) "<CR>"))

(fn viml->luaexp* [symb args]
  `(.. "luaeval('require(\"" *module-name* "\")[\"" ,(tostring symb) "\"](" (or args "") ")')"))


(fn get-lua-filename []
  "Get the lua filename of the current file"
  `(->
    *file*
    (vim.fn.substitute "\\.fnl$" ".lua" "")
    (vim.fn.substitute "fnl/" "lua/" "")))

(fn make-on-load [ns]
  `(.. "require(\"plugins." ,(tostring ns) "\").main()"))


(fn parse-command-args [args]
  (local out {:force true})
  (each [key val (pairs (or args {}))]
    (tset out key val))
  out)

(fn command! [lhs rhs args]
  `(vim.api.nvim_create_user_command ,lhs ,rhs ,(parse-command-args args)))

{: run-main
 : sym->name
 : from-iter
 : from-seq-iter
 : when-let
 : when-not
 : if-let
 : logx
 : viml->lua*
 : cviml->lua*
 : viml->luaexp*
 : get-lua-filename
 : make-on-load
 : command!}
