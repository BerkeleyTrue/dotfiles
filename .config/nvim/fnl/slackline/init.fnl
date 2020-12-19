(module slackline
  {:require {: utils
             : r
             : theme}
   :require-macros [macros]})

(defn- wrap-hl-group [name] (.. "%#" name "#"))
(def- space-separater "%=")

(defn- create-left-section []
  "left")

(defn- create-mid-section []
  "mid")

(def- sections
  [create-left-section
   create-mid-section])

(defn- create-line []
  (->>
    sections
    (r.map r.call)
    (r.join space-separater)))

(defn render-line []
  (let [line (create-line)]
    (set vim.wo.statusline line)))

(def- events
  ["FileType" "BufWinEnter" "BufReadPost" "BufWritePost" "BufEnter" "WinEnter" "FileChangedShellPost" "VimResized"])

(defn- add-augroup []
  (utils.augroup :slackline-au
    (->>
      events
      (r.map
        (fn [event]
          {: event
           :pattern :*
           :cmd (utils.viml->lua *module-name* (sym->name render-line))})))))


(defn main []
  (add-augroup))
