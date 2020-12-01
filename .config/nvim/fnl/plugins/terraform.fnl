(module plugins.terraform
  {:require {nvim aniseed.nvim
             r r}})

(defn main []
  (->>
    {:terraform_align 1
     :terraform_fold_sections 1
     :terraform_fmt_on_save 1}
    (r.to-pairs)
    (r.forEach
      (fn [[key val]]
        (tset nvim.g key val)))))
