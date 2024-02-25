(module lib.sexps
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    insertion lib.sexps.insertion
    operation lib.sexps.operation
    transformation lib.sexps.transformation}
   require {}
   require-macros [macros]})

(defn setup []
  (omap :af #(operation.around-form) {:desc "Select around a form"})
  (omap :ar #(operation.around-root) {:desc "Select around the root form"})
  (omap :if #(operation.in-form) {:desc "Select in a form"})
  (omap :ir #(operation.in-root) {:desc "Select in the root form"})
  (nmap :<leader>h #(insertion.head) {:desc "Insert into head of form"})
  (nmap :<leader>H #(insertion.tail {:desc "Insert into tail of form"}))

  (nmap :<localleader>i #(transformation.wrap-form) {:desc "Wrap form in a new form"})
  (nmap :<localleader>o #(transformation.unwrap-form) {:desc "Unwrap form"})
  (nmap :<localleader>O #(transformation.unwrap-element) {:desc "Unwrap element"})

  (nmap ">)" #(transformation.slurp-forward) {:desc "Slurp element forward"})
  (nmap "<)" #(transformation.barf-forward) {:desc "Barf element forward"})

  (nmap "<(" #(transformation.slurp-backward) {:desc "Slurp element backward"})
  (nmap ">(" #(transformation.barf-backward) {:desc "Barf element backward"}))

(defn main []
  (augroup :LibSexps
    {:event [:Filetype]
     :pattern :fennel
     :callback setup}))
