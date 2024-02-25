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
  ; operator pending
  (omap :af #(operation.around-form) {:desc "Select around a form"})
  (omap :ar #(operation.around-root-form) {:desc "Select around the root form"})
  (omap :if #(operation.in-form) {:desc "Select in a form"})
  (omap :ir #(operation.in-root-form) {:desc "Select in the root form"})

  ; selection
  (xmap :af #(operation.around-form) {:desc "Select around a form"})
  (xmap :ar #(operation.around-root-form) {:desc "Select around the root form"})
  (xmap :if #(operation.in-form) {:desc "Select in a form"})
  (xmap :ir #(operation.in-root-form) {:desc "Select in the root form"})

  (nmap :<i #(insertion.head) {:desc "Insert into head of form"})
  (nmap :>i #(insertion.tail {:desc "Insert into tail of form"}))

  (nmap :<leader>w #(transformation.wrap-form) {:desc "Wrap form in a new form"})
  (nmap :<leader>o #(transformation.unwrap-form) {:desc "Unwrap form"})
  (nmap :<leader>O #(transformation.unwrap-element) {:desc "Unwrap element"})

  (nmap ">)" #(transformation.slurp-forward) {:desc "Slurp element forward"})
  (nmap "<)" #(transformation.barf-forward) {:desc "Barf element forward"})

  (nmap "<(" #(transformation.slurp-backward) {:desc "Slurp element backward"})
  (nmap ">(" #(transformation.barf-backward) {:desc "Barf element backward"}))

(defn main []
  (augroup :LibSexps
    {:event [:Filetype]
     :pattern :fennel
     :callback setup}))
