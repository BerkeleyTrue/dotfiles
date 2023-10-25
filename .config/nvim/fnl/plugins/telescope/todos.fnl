(module plugins.telescope.todos
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [tc (md.prequire :todo-comments)]
    (tc.setup)))
