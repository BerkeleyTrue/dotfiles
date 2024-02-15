(module plugins.hlslens
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    hl utils.highlights
    cl lib.color
    p theme.palette}
   require-macros [macros]})


(defn override-lens [render pos-list nearest idx rel-idx]
  (let [abs-rel-idx (math.abs rel-idx)
        sfw (= (v searchforward) 1)
        dir (if (not= sfw (= rel-idx 1)) :N :n)
        indicator (if
                    (> abs-rel-idx 1) (: "%d%s" :format abs-rel-idx (if (not= sfw (> rel-idx 1)) :N :n))
                    (= abs-rel-idx 1) (if (not= sfw (= rel-idx 1)) :N :n)
                    "")
        cnt (length pos-list)
        (lnum col) (unpack (. pos-list idx))
        text (if nearest
               (if (= indicator "")
                 ""
                 (: "<> %s %d/%d" :format indicator idx cnt))
               (: "<> %s %d" :format indicator idx))
        chunks [[" " :Ignore] [text :HlSearchVirtualText]]]

    (render.setVirt 0 (- lnum 1) (- col 1) chunks nearest)))

(defn main []
  (n set_hl 0 :HlSearchVirtualText {:fg (cl.->hex 95 43.9 90)}) ; dark green
  (when-let [lens (md.prequire :hlslens)]
    (lens.setup {:override_lens override-lens})
    (nnoremap :n (.. "<CMD>execute('normal! '.v:count1.'n')<CR><CMD>lua require('hlslens').start()<CR>") {:silent true})
    (nnoremap :N (.. "<CMD>execute('normal! '.v:count1.'N')<CR><CMD>lua require('hlslens').start()<CR>") {:silent true})
    (nnoremap :* (.. "*<CMD>lua require('hlslens').start()<CR>") {:silent true})
    (nnoremap :# (.. "#<CMD>lua require('hlslens').start()<CR>") {:silent true})
    (nmap :<ESC><ESC> ":noh<CR>" {:silent true})))
