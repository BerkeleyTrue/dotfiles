(module plugins.hlslens
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(var lens-bak nil)

(defn start []
  (. (require :hlslens) :start))

(defn override-lens [render pos-list nearest idx]
  (let [_ rel-idx
        (lnum col) (unpack (. pos-list idx))]
    (var (text chunks) nil)
    (if nearest
      (do
        (set text (: "[%d/%d]" :format idx (length pos-list)))
        (set chunks {1 {1 " " 2 :Ignore} 2 {1 text 2 :VM_Extend}}))
      (do
        (set text (: "[%d]" :format idx))
        (set chunks {1 {1 " " 2 :Ignore} 2 {1 text 2 :HlSearchLens}})))

    (render.setVirt 0 (- lnum 1) (- col 1) chunks nearest)))

(defn start-vm []
  (let [lens (require :hlslens)
        config (require :hlslens.config)]

    (set lens-bak config.override_lens)
    (set config.override_lens override-lens)
    (lens.start true)))

(defn exit-vm []
  (let [lens (require :hlslens)
        config (require :hlslens.config)]

    (set config.override_lens lens-bak)
    (lens.start true)))

(defn main []
  (when-let [lens (md.prequire :hlslens)]
    (utils.nnoremap :n (.. "<CMD>execute('normal! '.v:count1.'n')<CR>" (cviml->lua* start)) {:silent true})
    (utils.nnoremap :N (.. "<CMD>execute('normal! '.v:count1.'N')<CR>" (cviml->lua* start)) {:silent true})
    (utils.nnoremap :* (.. "*" (cviml->lua* start)) {:silent true})
    (utils.nnoremap :# (.. "#" (cviml->lua* start)) {:silent true})
    (utils.nmap :<ESC><ESC> ":noh<CR>" {:silent true})
    (augroup
      :VMLens
      {:event :User
       :pattern :visual_multi_start
       :callback start-vm}
      {:event :User
       :pattern :visual_multi_exit
       :callback exit-vm})))
