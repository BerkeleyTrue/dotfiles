(module plugins.package-info
  {require
   {r r
    utils utils
    md utils.module}
   require-macros [macros]})

(defn main []
  (when-let [pi (md.prequire :package-info)]
    (pi.setup
      {:colors
       {:up_to_date "#6272A4"
        :outdated "#8BE9FD"}

       :autostart true})))
