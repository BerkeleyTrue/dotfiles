(module utils.module
  {:require
   {utils utils}})

(defn prequire [namespace]
  "Safe require a module. Errors are caught and printed.
  (prequire :utils)"
  (let [(ok res) (pcall require namespace)]
    (if
      ok res
      (print (.. "Could not load module "
                 (tostring namespace)
                 ": " (tostring res))))))

(defn
  ppackadd
  [packname]
  "Make a safe call to packadd.
  (ppackadd packname)"
  (let [(ok res) (pcall utils.ex.packadd packname)]
    (if
      ok (or res true)
      (print (.. "Could not load package "
                 (tostring packname)
                 ": " (tostring res))))))

(defn packadd-n-require
  [name namespace]
  "Make a safe call to packadd and require.
  (safe-packadd packname namespace?)
  (safe-packadd :some-package.nvim :some-package)"
  (let [namespace (or name namespace)]
    (when (ppackadd name)
      (prequire namespace))))
