(module plugins.colorizer
  {require {a aniseed.core
            nvim aniseed.nvim}})

(defn main []
  (let [(ok res) (pcall nvim.ex.packadd :nvim-colorizer.lua)]
    (if
      ok (let [colorizer (require :colorizer)]
           (colorizer.setup))
      (print "Colorizer not found in path"))))

(comment (main))
