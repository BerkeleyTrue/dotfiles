(module :plugins.js
  {:require {a aniseed.core
             r r
             nvim aniseed.nvim}})


(defn main []
  (->>
    {:javascript_plugin_jsdoc 1
     :javascript_plugin_flow 1}
    (r.to-pairs)
    (r.forEach
      (fn [[key val]]
        (tset nvim.g key val)))))
