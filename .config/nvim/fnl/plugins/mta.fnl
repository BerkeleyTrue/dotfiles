(module plugins.mta
  {:require {utils utils}})

(defn main []
  (utils.set-nvim-g! {:mta_filetypes {:html  1
                                      :xhtml  1
                                      :xml  1
                                      :javascript.jsx  1}

                      :mta_use_matchparen_group 1}))
