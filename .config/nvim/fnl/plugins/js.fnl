(module :plugins.js
  {:require {utils utils}})


(defn main []
  (utils.set-nvim-g!
    {:javascript_plugin_jsdoc 1
     :javascript_plugin_flow 1
     :vim_jsx_pretty_highlight_close_tag 1}))
