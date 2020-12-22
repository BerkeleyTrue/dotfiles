(module slackline
  {:require {: utils
             : r
             :t theme
             :a aniseed.core
             :mode-map slackline.mode-map}
   :require-macros [macros]})

; status line utils
(def- space-separater "%=")
(defn- hl-comp [str]
  (.. "%#" str "#"))

(defn- format-hl-mode [name]
  (->>
    name
    (r.deburr)
    (.. "berks mode ")
    (r.pascal-case)))

;; componends
(defn- create-hl-group [name]
  (->
    name
    (format-hl-mode)
    (hl-comp)))

(defn- expression-comp [expr] (.. "%8{" expr "}"))
(defn- reset-hl-comp [str] (.. str (hl-comp :StatusLine)))

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
  (.. (if (= 1 (utils.fn.getbufvar (utils.fn.bufnr "%") "&mod")) " " "")))

(defn- create-left-section []
  (->>
    [mode-comp
     dir-comp
     file-modified-comp]
    (r.map r.call)
    (r.join " ")))

(defn- ale-comp [type]
  (when (> (utils.fn.exists ":ALELint") 0)
    (let [err? (= type :err)
          {:error err
           : style_error
           : total} (utils.fn.ale#statusline#Count (utils.fn.bufnr "%"))
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


(defn- create-mid-section []
  (->>
    [ale-error-comp
     ale-warning-comp]
    (r.map r.call)
    (r.join " ")))

(def- sections
  [create-left-section
   create-mid-section])

(defn- create-line []
  (->>
    sections
    (r.map r.call)
    (r.join space-separater)))

(defn render-active-line []
  (create-line))

(def- active-status-func (utils.viml-fn-bridge *module-name* (sym->name render-active-line)))

(defn active-line []
  (set vim.wo.statusline "")
  (set vim.wo.statusline (.. "%!" active-status-func "()")))

(def- events
  [:FileType
   :BufWinEnter
   :BufReadPost
   :BufWritePost
   :BufEnter
   :FocusGained
   :WinEnter
   :FileChangedShellPost
   :VimResized])

(defn- add-augroup []
  (utils.augroup :slackline-au
    (->>
      events
      (r.map
        (fn [event]
          {: event
           :pattern :*
           :cmd (utils.viml->lua *module-name* (sym->name active-line))})))))

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
