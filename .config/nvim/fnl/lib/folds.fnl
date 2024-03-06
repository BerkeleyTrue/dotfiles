(module lib.folds
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def- data
  {:folds []
   :prev-end nil})

(defn collect-folds []
  (let [line (vim.fn.line ".")
        end (vim.fn.foldclosedend line)]
    (when (or (r.nil? data.prev-end) ; first fold
              (not (= end data.prev-end))) ; new none-nested fold
      (tset data :prev-end end)
      (set data.folds (r.conj data.folds {:start line :end end})))))

(defn do-fold-collection []
  (tset data :folds [])
  (tset data :prev-end nil)
  ; save cursor pos
  (let [pos (n win-get-cursor 0)]
    (command folddoclosed (viml->lua* collect-folds))
    (n win-set-cursor 0 pos)
    (let [_folds data.folds]
      _folds)))

(defn in-fold? [linenr]
  "Check if the given line number is in a fold, and return the fold range."
  (let [folds (do-fold-collection)]
    (var in? false)
    (var _end nil)
    (var _start nil)
    (each [_ {: start : end} (ipairs folds) &until in?]
      (when (and (>= linenr start) (<= linenr end))
        (set in? true)
        (set _end end)
        (set _start start)))
    (values in? {:start _start :end _end})))

(comment (in-fold? 14))

(defn count-folded-lines [linenr]
  "Count how many lines are folded upto the given line number, 
  or the whole file if no line number is given.
  TODO: count within range, not just upto the line number."
  (let [folds (do-fold-collection)
        linenr (or linenr (vim.fn.line "$"))]
    (var count 0)
    (var found false)
    (each [_ {: start : end} (ipairs folds) &until found]
      (when (<= start linenr)
        (if (<= end linenr)
          (set count (+ count (- end start))) ; don't count the fold line itself
          (do
            (set count (+ count (- linenr start)))
            (set found true)))))
    count))

(comment 
  (count-folded-lines 48))

(defn main []
  (command! :CollectFolds (fn print-folds [] (a.pr (do-fold-collection)))))
