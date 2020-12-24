(module slackline
  {:require {: utils
             : r
             :t theme
             :a aniseed.core
             :mode-map slackline.mode-map}
   :require-macros [macros]})

; general utils
(def- space-separater "%=")
(defn- hl-comp [str]
  (.. "%#" str "#"))

(defn- format-hl-mode [name]
  (->>
    name
    (r.deburr)
    (.. "berks mode ")
    (r.pascal-case)))

;; components utils
(defn- create-hl-group [name]
  (->
    name
    (format-hl-mode)
    (hl-comp)))

(defn- expression-comp [expr] (.. "%8{" expr "}"))
(defn- reset-hl-comp [str] (.. str (hl-comp :StatusLine)))

;; sections
(defn- compose-comps [comps]
  #(->>
     comps
     (r.map r.call)
     (r.join " ")))

(defn- create-line [sections]
  (->>
    sections
    (r.map r.call)
    (r.join space-separater)))

(defn- mode-comp []
  (->>
    mode-map.m
    (r.to-pairs)
    (r.reduce
      (fn [acc [mode {: name : fg : bg : case-insensitive}]]
        (..
          acc
          (create-hl-group name fg bg)
          (expression-comp (.. "(mode()==#\"" mode "\")?'" name "':''"))))
      "")
    (reset-hl-comp)))

(defn- dir-comp []
  (let [wd (utils.fn.expand "%:.")
        pd (utils.fn.expand "%:.:h:t")
        fln (utils.fn.expand "%:t")]
    (if
      (< (length wd) 14) (.. " " wd)
      (.. "../" pd "/" fln))))

(defn- file-modified-comp []
  (.. (if (= 1 (utils.fn.getbufvar (utils.fn.bufnr) "&mod")) " " "")))

(def- create-left-section
  (compose-comps
    [mode-comp
     dir-comp
     file-modified-comp]))

(defn- ale-comp [type]
  (when (> (utils.fn.exists ":ALELint") 0)
    (let [err? (= type :err)
          {:error err
           : style_error
           : total} (utils.fn.ale#statusline#Count (utils.fn.bufnr))
          errs (+ err style_error)
          warns (- total errs)
          show? (if err? (> errs 0) (> warns 0))
          render (.. (if err? (.. " " errs) (.. " " warns)))]
      (if show? render ""))))

(defn- ale-error-comp []
  (reset-hl-comp
    (->
      :BerksRedInverse
      (hl-comp)
      (.. (ale-comp "err")))))

(defn- ale-warning-comp []
  (reset-hl-comp
    (->
      :BerksYellow
      (hl-comp)
      (.. (ale-comp "warning")))))


(def- create-mid-section
  (compose-comps
    [ale-error-comp
     ale-warning-comp]))

(defn render-active-line []
  (create-line
    [create-left-section
     create-mid-section]))

(defn render-inactive-line [num]
  (print num)
  (create-line
    [(compose-comps [file-modified-comp])]))

(def- active-status-func (utils.viml-fn-bridge *module-name* (sym->name render-active-line)))
(def- inactive-status-func (utils.viml-fn-bridge *module-name* (sym->name render-inactive-line)))

(defn active-line []
  (set vim.wo.statusline "")
  (set vim.wo.statusline (.. "%!" active-status-func "()")))

(defn inactive-line []
  (set vim.wo.statusline "")
  (set vim.wo.statusline (.. "%!" inactive-status-func "()")))

(def- active-events
  [
   :BufWinEnter
   :BufReadPost
   :BufWritePost
   :BufEnter
   :FocusGained
   :WinEnter])

(def- inactive-events
  [
   :BufLeave
   :FocusLost
   :WinLeave])

(defn- add-augroup []
  (utils.augroup :slackline-au
    [(->>
       active-events
       (r.reduce #(.. $1 (if (or (r.empty? $1) (r.empty? $2)) "" ",") $2) "")
       ((fn [event]
         {: event
          :pattern :*
          :cmd (utils.viml->lua *module-name* (sym->name active-line))})))
     (->>
       inactive-events
       (r.reduce #(.. $1 (if (or (r.empty? $1) (r.empty? $2)) "" ",") $2) "")
       ((fn [event]
         {: event
          :pattern :*
          :cmd (utils.viml->lua *module-name* (sym->name inactive-line))})))]))

(defn init []
  (->>
    mode-map.m
    (r.vals)
    (r.for-each
      (fn [{: name : fg : bg}]
        (t.add-group (format-hl-mode name) fg bg)))))

(defn main []
  (init)
  (add-augroup))
