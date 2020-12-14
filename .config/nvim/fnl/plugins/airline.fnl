(module plugins.airline
  {:require {nvim aniseed.nvim
             utils utils}
   :require-macros [macros]})

(defn get-dir-prompt []
  (let [wd (nvim.fn.expand "%:.")
        pd (nvim.fn.expand "%:.:h:t")
        fln (nvim.fn.expand "%:t")]
    (if
      (< (length wd) 14) (.. " " wd)
      (.. "../" pd "/" fln))))

(def- get-dir-prompt-viml-name (utils.viml-fn-bridge *module-name* (sym->name get-dir-prompt)))


(defn- create-section [side? confs]
  (let [side (if (and (= (type side?) "string") (> (length side?) 0))
               (.. "_" side?)
               "")
        name (.. "airline#section#create" side)]
    (nvim.fn.call name [confs])))

(defn airline-init []
  (utils.set-nvim-g! {:airline_section_a (create-section "" [:mode :crypt :paste :iminsert])
                      :airline_section_b (create-section :left [:hunks])
                      :airline_section_c (create-section :left [:pwd :readonly])
                      :airline_section_x (create-section :right [:bookmark :tagbar :vista :gutentags :omnisharp :grepper])
                      :airline_section_y (create-section :left [:filetype])
                      :airline_section_z (create-section "" [:linenr :maxlinenr ::%3v])}))
(defn main []
    (utils.set-nvim-g! {:airline_theme "dracula"
                        :airline_powerline_fonts   1
                        :airline_detect_spell 0
                        :airline_symbols.linenr ""
                        :airline_symbols.maxlinenr ""

                        :airline_left_sep   ""
                        :airline_left_alt_sep   ""
                        :airline_right_sep   ""
                        :airline_right_alt_sep   ""

                        ; Tab buffer list above the window
                        :airline#extensions#tabline#enabled   0

                        :airline#extensions#ale#enabled   1
                        :airline#extensions#ale#warning_symbol   ""
                        :airline#extensions#ale#error_symbol   "✗"

                        :airline#extensions#coc#enabled   0
                        :airline#extensions#coc#warning_symbol   ""
                        :airline#extensions#coc#error_symbol   "✗"

                        :airline#extensions#whitespace#enabled   0})

  (let [(ok err) (pcall
                   nvim.fn.call
                   :airline#parts#define_function
                   [:pwd get-dir-prompt-viml-name])]


    (when (not ok) (print (.. "couldn't execute airline define func: " err))))
  (utils.augroup :airline-au [{:event :VimEnter :pattern :* :cmd (utils.viml->lua *module-name* :airline-init)}]))
