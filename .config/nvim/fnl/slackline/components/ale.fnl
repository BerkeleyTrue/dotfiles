(module slackline.components.ale
  {:require {: utils}})

; (defn- ale-comp [type]
;   (when (> (utils.fn.exists ":ALELint") 0)
;     (let [err? (= type :err)
;           {:error err
;            : style_error
;            : total} (utils.fn.ale#statusline#Count (utils.fn.bufnr))
;           errs (+ err style_error)
;           warns (- total errs)
;           show? (if err? (> errs 0) (> warns 0))
;           render (.. (if err? (.. " " errs) (.. " " warns)))]
;       (if show? render ""))))

; (defn- ale-error-comp []
;   (reset-hl-comp
;     (->
;       :BerksRedInverse
;       (hl-comp)
;       (.. (ale-comp "err")))))

; (defn- ale-warning-comp []
;   (reset-hl-comp
;     (->
;       :BerksYellow
;       (hl-comp)
;       (.. (ale-comp "warning")))))

(defn main [])
