(module plugins.hop
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

;-- place this in one of your configuration file(s)
(defn hop-find []
  ((. (require :hop)
    :hint_char1)
   {:direction (. (require :hop.hint) :HintDirection :AFTER_CURSOR)
    :current_line_only true}))

(defn hop-vertical-j []
  ((. (require :hop)
    :hint_vertical)
   {:direction (. (require :hop.hint) :HintDirection :AFTER_CURSOR)}))

(defn hop-vertical-k []
  ((. (require :hop)
    :hint_vertical)
   {:direction (. (require :hop.hint) :HintDirection :BEFORE_CURSOR)}))

(defn main []
  (when-let [hop (md.packadd-n-require :hop.nvim :hop)]
    (hop.setup)
    (utils.nmap :f (cviml->lua* hop-find))
    (utils.nmap :<leader>j (cviml->lua* hop-vertical-j))
    (utils.nmap :<leader>k (cviml->lua* hop-vertical-k))))
