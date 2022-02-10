(module plugins.aniseed
  {require
   {a aniseed.core
    anenv aniseed.env
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn compile-fnl []
  (print :recompiling)
  (anenv.init
    {:force true
     :init :foo}))

(defn main []
  (utils.ex.command_
    :AniseedCompile
    (utils.viml->lua *module-name* (sym->name compile-fnl))))
