(module plugins.completion
  {:require {: r
             : utils
             :conjure-compe completion-sources.conjure}


   :require-macros [macros]})

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-compe)]
    (if
      ok (let [compe (require :compe)]
           (compe.register_source :conjure conjure-compe.source)
           (compe.setup
             {:enabled true
              :debug true
              :min_length 1
              :preselect :enable
              :throttle_time 250
              :source_timeout 500
              :incomplete_delay 250
              :allow_prefix_unmatch false
              :max_abbr_width 100
              :max_kind_width 100
              :max_menu_width 100
              :documentation true
              :source
              {:path true
               :buffer true
               :calc true
               :spell true
               :vsnip false
               :nvim_lsp true
               :nvim_lua true
               :ultisnips true
               :conjure true
               :nvim_treesitter true}}))

      (print "compe not found in path"))))
