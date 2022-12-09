(module plugins.no-neck-pain
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main [])
  ;; (when-let [nnp (md.prequire :no-neck-pain)]
  ;;   (nnp.setup
  ;;     {:width 100})
  ;;   (augroup :NoNeckPainEnter
  ;;     {:event :VimEnter
  ;;      :pattern :*
  ;;      :callback (fn [] (vim.schedule (fn [] (nnp.start))))})))
