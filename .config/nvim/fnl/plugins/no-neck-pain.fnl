(module plugins.no-neck-pain
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(defn main []
  (when-let [nnp (md.prequire :no-neck-pain)]

    (nnp.setup
      {:width 120
       :buffers
       {:showName true}})

    (augroup :NoNeckPainEnter
      {:event :VimEnter
       :pattern :*
       :callback
       (fn []
         (vim.schedule
           (fn []
             (when (and
                     (not= (bo filetype) "dashboard")
                     (= (. _G :NoNeckPain.state) nil))
               (nnp.enable)))))}

      {:event :BufWinEnter
       :pattern :*
       :callback
       (fn []
          (vim.schedule
            (fn []
              (when (and
                       (not= (bo filetype) "dashboard")
                       (. _G :NoNeckPainLoaded)
                       (= (. (. _G :NoNeckPain) :state) nil))
                (nnp.enable)))))})))
