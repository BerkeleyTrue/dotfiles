(module plugins.completion
  {:require {: r
             : utils}

   :require-macros [macros]})

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-compe)]
    (if
      ok (let [compe (require :compe)]
           (compe.setup {:enabled true
                         :debug true
                         :min_length 1
                         :preselect :enable
                         :throttle_time 250
                         :source_timeout 500
                         :incomplete_delay 250
                         :allow_prefix_unmatch false
                         :source
                         {:path true
                          :buffer true
                          :vsnip true
                          :nvim_lsp true
                          :nvim_lua true
                          :ultisnips true}}))


      (print "compe not found in path"))))
