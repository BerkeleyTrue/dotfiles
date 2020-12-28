(module slackline
  {:require {: utils
             : r
             :t theme
             :a aniseed.core
             :mode slackline.components.mode
             :dir slackline.components.dir
             :modified slackline.components.modified}
   :require-macros [macros]})

(def registry {})

(defn register [name] (tset registry name true))

(defn registered? [name] (or (. registry name) false))

(defn active-comps [args]
  (mode.main
    (partial dir.main (partial modified.main nil))
    args))

(defn render-comp [spec line]
  (if (r.nil? spec) line
    (let [{: name :init init? : render : next :props props} (or spec {})
          init (if (r.function? init?) init? r.noop)]
      (when (not (registered? name))
        (register name)
        (init))
      (render-comp next (.. line (or (render (or props {})) (.. "got nil from " name)))))))

(defn render-slackline [active]
  (render-comp
    (active-comps {: active})
    ""))


(def- render-slackline-bridge (utils.viml-fn-bridge *module-name* (sym->name render-slackline)))

(defn init-slackline [args]
  (let [{: active} (or args {})]
    (set vim.wo.statusline "")
    (set vim.wo.statusline (.. "%!" render-slackline-bridge "(" (if active "v:true" "v:false") ")"))))

(def- active-events
  [:BufWinEnter
   :BufReadPost
   :BufWritePost
   :BufEnter
   :FocusGained
   :WinEnter])

(def- inactive-events
  [:BufLeave
   :FocusLost
   :WinLeave])

(defn- add-augroup []
  (utils.augroup :slackline-au
    [{:event active-events
      :pattern :*
      :cmd (utils.viml->lua *module-name* (sym->name init-slackline) {:args "{ active = true }"})}
     {:event inactive-events
      :pattern :*
      :cmd (utils.viml->lua *module-name* (sym->name init-slackline) {:args "{active = false}"})}]))

(defn main []
  (add-augroup))
