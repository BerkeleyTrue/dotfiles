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


(var lens-bak nil)

(defn start []
  (. (require :hlslens) :start))

; TODO: add specific visual multi override-lens
(defn override-lens [render pos-list nearest idx]
  (let [(lnum col) (unpack (. pos-list idx))
        text (if nearest "" (: "[%d]" :format idx))
        chunks [[" " :Ignore] [text :HlSearchVirtualText]]]

    (render.setVirt 0 (- lnum 1) (- col 1) chunks nearest)))

(defn start-vm []
  (when-let [lens (require :hlslens)]
    (lens.start true)))

(defn exit-vm []
  (when-let [lens (require :hlslens)]
    (lens.start true)))

(defn main []
  (n set_hl 0 :HlSearchVirtualText {:fg (cl.hsl->hex 95 43.9 90)}) ; dark green
  (when-let [lens (md.prequire :hlslens)]
    (lens.setup {:override_lens override-lens})
    (nnoremap :n (.. "<CMD>execute('normal! '.v:count1.'n')<CR>" (cviml->lua* start)) {:silent true})
    (nnoremap :N (.. "<CMD>execute('normal! '.v:count1.'N')<CR>" (cviml->lua* start)) {:silent true})
    (nnoremap :* (.. "*" (cviml->lua* start)) {:silent true})
    (nnoremap :# (.. "#" (cviml->lua* start)) {:silent true})
    (nmap :<ESC><ESC> ":noh<CR>" {:silent true})
    (augroup
      :VMLens
      {:event :User
       :pattern :visual_multi_start
       :callback start-vm}
      {:event :User
       :pattern :visual_multi_exit
       :callback exit-vm})))
