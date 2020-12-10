(local module-sym (gensym))

(fn safe-require [name]
  `(let [(ok# res#) (pcall require ,name)]
     (if
       ok# res#
       (print (.. "Couldn't load " ,name)))))

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

{:safe-require safe-require
 :run-main run-main}
