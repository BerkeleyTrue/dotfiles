(module plugins.easy-motion
  {require
   {: r
    : utils}
   require-macros [macros]})


(def- packages [:vim-easymotion])

(defn main []
  (->
    {:EasyMotion_smartcase   1
     :EasyMotion_startofline 0}
    (utils.set-nvim-g!))

  (let [ok (->>
             packages
             (r.reduce
               #(let [ok (pcall utils.ex.packadd $2)]
                 (if
                  ;; if ok and past val is true, send true, otherwise send false
                   ok (and $1 true)
                   (do
                     (print (.. "Could not load " $2))
                     false)))
               true))]
    (when ok
      ;(utils.nmap :s "<Plug>(easymotion-s2)")
      (utils.nmap "t" "<Plug>(easymotion-t2)")
      (utils.omap "/" "<Plug>(easymotion-tn)")

      ; different highlight method and have some other features )
      (utils.amap "n" "<Plug>(easymotion-next)")
      (utils.amap "N" "<Plug>(easymotion-prev)")
      ; Search in line
      ; map <Leader>l <Plug>(easymotion-lineforward)
      (utils.amap "<leader>j" "<Plug>(easymotion-j)")
      (utils.amap "<leader>k" "<Plug>(easymotion-k)")
      (utils.amap "<leader>h" "<Plug>(easymotion-linebackward)")

      (utils.nmap :f "<Plug>(easymotion-fl)")
      (utils.vmap :f "<Plug>(easymotion-fl)")
      (utils.omap :f "<Plug>(easymotion-fl)")

      (utils.nmap :F "<Plug>(easymotion-Fl)")
      (utils.vmap :F "<Plug>(easymotion-Fl)")
      (utils.omap :F "<Plug>(easymotion-Fl)")

      (utils.nmap :t "<Plug>(easymotion-tl)")
      (utils.vmap :t "<Plug>(easymotion-tl)")
      (utils.omap :t "<Plug>(easymotion-tl)")

      (utils.nmap :T "<Plug>(easymotion-Tl)")
      (utils.vmap :T "<Plug>(easymotion-Tl)")
      (utils.omap :T "<Plug>(easymotion-Tl)"))
    (when (not ok) (print "Could not load easy-motion confs"))))
