(module plugins.lightbulb
  {:require {: utils}
   :require-macros [macros]})


(defn light-me []
  (: (require :nvim-lightbulb) :update_lightbulb))


(defn main []
  (let [(ok res) (pcall utils.ex.packadd :nvim-lightbulb)]
    (if (not ok) (print (.. "Could not load nvim-lightbulb: " (tostring res)))
      (let [(ok bulb) (pcall require :nvim-lightbulb)]
        (if (not ok) (print (.. "require: ") bulb)
          (bulb.update_lightbulb)
          (utils.augroup :ligtbulb-au
            [{:event [:CursorHold :CursorHoldI]
              :pattern :*
              :cmd (utils.viml->lua *module-name* (sym->name light-me))}]))))))
