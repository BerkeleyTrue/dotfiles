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
              (print (.. "Could not load module"
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

{:run-main run-main
 :sym->name sym->name
 :from-iter from-iter
 :from-seq-iter from-seq-iter
 :when-let when-let}
