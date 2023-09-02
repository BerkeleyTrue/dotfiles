(module plugins.hop
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn main []
  (when-let [hop (md.prequire :hop)]
    (let [hint (require :hop.hint)
          hop-find (fn hop-find []
                     (hop.hint_char1
                       {:direction hint.HintDirection.AFTER_CURSOR
                        :current_line_only true}))

          hop-till (fn hop-till []
                     (hop.hint_char1
                       {:direction hint.HintDirection.AFTER_CURSOR
                        :current_line_only true
                        :hint_offset -1}))
          hop-vertical-j (fn hop-vertical-j []
                           (hop.hint_vertical
                             {:direction hint.HintDirection.AFTER_CURSOR}))

          hop-vertical-k (fn hop-vertical-k []
                           (hop.hint_vertical
                             {:direction hint.HintDirection.BEFORE_CURSOR}))]
      (nmap :f hop-find)
      (vmap :f hop-find)
      (vmap :t hop-till)
      (nmap :<leader>j hop-vertical-j)
      (nmap :<leader>k hop-vertical-k)
      (hop.setup))))
