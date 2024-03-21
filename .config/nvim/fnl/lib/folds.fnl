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

(def- fold-viml-command (viml->lua* collect-folds))

(defn do-fold-collection []
  (tset data :folds [])
  (tset data :prev-end nil)
  ; save cursor position, as collect fold will move cursor
  (let [pos (n win-get-cursor 0)]
    ; keepjumps is used to prevent changes to jumplist
    (command keepjumps :folddoclosed fold-viml-command)
    ; restore cursor position
    (n win-set-cursor 0 pos)
    (let [_folds data.folds]
      _folds)))

(defn overlap-range [start end folds]
  "Return the range of folds that overlap with the given range."
  (var out [])
  (var end-found? nil)
  (each [_ {:start fstart :end fend} (ipairs folds) &until end-found?]
    (when (or (<= start fstart end) ; if start is in fold
              (<= start fend   end)) ; if end is in fold
      (table.insert out {:start fstart :end fend})
      (when (<= end fend)
        (set end-found? true))))
  out)

(comment 
  (overlap-range 5 10 [{:start 1 :end 3} {:start 5 :end 7} {:start 9 :end 11}])
  (overlap-range 1 69 [{:start 1 :end 5} {:start 9 :end 20} {:start 25 :end 30}]))

(defn overlap-fold [start end folds]
  "Return the range of folds that overlap with the given range."
  (var out [])
  (var end-found? nil)
  (each [_ {:start fstart :end fend} (ipairs folds) &until end-found?]
    (when (or (<= fstart start fend) ; if start is in range
              (<= fstart end   fend)) ; if end is in range
      (table.insert out {:start fstart :end fend})
      (when (<= end fend)
        (set end-found? true))))
  out)

(comment
  (overlap-fold 11 11 [{:start 6 :end 12}]))

(defn in-fold? [linenr folds]
  "Check if the given line number is in a fold, and return the fold range."
  (let [folds (->>
                (or folds (do-fold-collection))
                (overlap-fold linenr linenr))]
    (when-not (r.empty? folds)
      (r.head folds))))

(comment (in-fold? 11 [{:start 6 :end 12}]))

(defn count-folded-lines [{: start : end} folds]
  "Count how many lines are folded from start upto the given line end line numbers, 
  or the whole file if no start or end line number is given."
  (let [start (or start 1)
        end (or end (vf line "$"))]
    (->>
      (or folds (do-fold-collection))
      (overlap-range start end) 
      (r.reduce
        (fn [acc {:start fstart :end fend}]
          (let [end (if (<= start fend end) fend end)
                start (if (<= start fstart end) fstart start)]
            (+ acc (- end start))))
        0))))

(comment 
  (count-folded-lines {:start 1 :end 69} [{:start 1 :end 5} {:start 9 :end 20} {:start 25 :end 30}])
  (count-folded-lines {:start 14 :end 27} [{:start 1 :end 5} {:start 9 :end 20} {:start 25 :end 30}]))

(defn main []
  (command! :CollectFolds (fn print-folds [] (a.pr (do-fold-collection)))))
