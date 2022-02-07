(module plugins.terraform
  {require {utils utils}})


(defn main []
  (utils.set-nvim-g! {:terraform_align 1
                      :terraform_fold_sections 1
                      :terraform_fmt_on_save 1}))
