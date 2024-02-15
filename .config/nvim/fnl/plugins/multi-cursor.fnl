(module plugins.multi-cursor
  {autoload
   {a aniseed.core
    r r
    utils utils
    hlslens hlslens
    md utils.module}
   require-macros [macros]})

(var lens-bak nil)

(defn override-lens [render pos-list nearest idx _]
  (let [(lnum col) (unpack (. pos-list idx))
        text (if nearest (: "[%d/%d]" :format idx (length pos-list)) (: "<%d>" :format idx))
        chunks [[" " :Ignore] [text (if nearest :VM_Extend :HlSearchVirtualText)]]]

    (render.setVirt 0 (- lnum 1) (- col 1) chunks nearest)))

; TODO: only seems to work on the first time
(defn start-w-override []
  (when-let [config (md.prequire :hlslens.config)]
    (set lens-bak config.override_lens)
    (set config.override_lens override-lens)
    (hlslens.start)))

(defn exit-w-override []
  (when-let [config (md.prequire :hlslens.config)]
    (set config.override_lens lens-bak)
    (set lens-bak nil)
    (hlslens.start)))

(defn main []
  (g! VM_quit_after_leaving_insert_mode 0)
  (g! VM_set_statusline 0)
  (augroup
    :VMLens
    {:event :User
     :pattern :visual_multi_start
     :callback start-w-override}
    {:event :User
     :pattern :visual_multi_exit
     :callback exit-w-override}))
