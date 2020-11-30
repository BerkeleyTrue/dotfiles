(local module-sym (gensym))

(fn safe-require [name]
  `(let [(ok# res#) (pcall require ,name)]
     (if
       ok# res#
       (print (.. "Couldn't load " ,name)))))

(fn run-main [name]
  `(-?>
     ,name
     (safe-require)
     ((fn [mod#] ((. mod# :main))))))

{:safe-require safe-require
 :run-main run-main}
