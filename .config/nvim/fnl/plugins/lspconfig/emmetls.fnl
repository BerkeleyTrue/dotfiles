(module plugins.lspconfig.emmetls
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn get-config []
  {:filetypes
   [:html :css :javascript :typescriptreact :pug]})
