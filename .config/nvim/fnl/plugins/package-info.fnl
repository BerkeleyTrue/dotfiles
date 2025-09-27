(module plugins.package-info
  {require
   {r r
    utils utils
    md utils.module}
   require-macros [macros]})

(defn main []
  (when-let [pi (md.prequire :package-info)]
    (pi.setup
      {:highlights
       {:up_to_date {:fg "#6272A4"}
        :outdated {:fg "#8BE9FD"}}

       :autostart true})))
