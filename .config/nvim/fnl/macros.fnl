(local module-sym (gensym))

(fn safe-require [name]
  `(let [(ok# res#) (pcall require ,name)]
     (if
       ok# res#
       (print (.. "Couldn't load " ,name ": " res#)))))

(fn run-main [name ...]
  `(let [args# ,[...]]
     (-?>
       ,name
       (safe-require)
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
  (tostring a-sym))

(fn from-iter [iter-fact]
  "(from-iter (pairs {:apple \"red\" :orange \"orange\"})) ;=> {:red \"apple\" :orange \"orange\"}"
  `(let [tbl# {}]
     (each [k# v# ,iter-fact]
       (tset tbl# k# v#))
     tbl#))

(fn from-seq-iter [iter-fact]
  "(from-seq-iter (ipairs [:apple :orange])) ;=> [:apple :orange]"
  `(let [tbl# []]
     (each [_# v# ,iter-fact]
       (table.insert tbl# v#))
     tbl#))

{:safe-require safe-require
 :run-main run-main
 :sym->name sym->name
 :from-iter from-iter
 :from-seq-iter from-seq-iter}
