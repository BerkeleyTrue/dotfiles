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
  (tset data :folds {})
  (tset data :prev-end nil)
  (command folddoclosed (viml->lua* collect-folds))
  (let [_folds data.folds]
    _folds))

(defn in-fold? [linenr]
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

(defn main []
  (command! :CollectFolds (fn print-folds [] (a.pr (do-fold-collection)))))
