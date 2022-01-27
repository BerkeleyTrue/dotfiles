(module plugins.package-info
  {require {: r
            : utils}

   require-macros [macros]})

(defn main []
  (let [(ok res) (pcall utils.ex.packadd :package-info.nvim)]

    (if
      ok (let [pi (require :package-info)]
           (pi.setup
             {:colors
              {:up_to_date "#6272A4"
               :outdated "#8BE9FD"}

              :autostart true}))

      (print "package-info not found in path"))))
