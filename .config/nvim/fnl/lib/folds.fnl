(module lib.folds
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def- data
  {:folds {}
   :prev-end nil})

; TODO: also check for starting line number
(defn collect-folds []
  (let [line (vim.fn.line ".")
        end (vim.fn.foldclosedend line)]
    (when (not data.prev-end) ; first fold
      (tset data :prev-end end)
      (tset data.folds line end))
    (when (not (= end data.prev-end)) ; new none-nested fold
      (tset data :prev-end end)
      (tset data.folds line end))))

(defn do-fold-collection []
  (tset data :folds {})
  (tset data :prev-end nil)
  (command folddoclosed (viml->lua* collect-folds))
  (let [_folds data.folds]
    _folds))

(defn main []
  (command! :CollectFolds (fn print-folds [] (a.pr (do-fold-collection)))))
