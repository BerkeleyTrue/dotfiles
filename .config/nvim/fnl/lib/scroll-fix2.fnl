(module lib.scroll-fix2
  {autoload
   {a aniseed.core
    r r
    fld lib.folds}
   require {}
   import-macros []
   require-macros [macros]})

(var enabled false)
(var debug false)

(defn- set-top-of-window [line wintable]
  "set the top line of the window to be `line`
   this effectively scrolls the window"
  (-> wintable
    (r.assoc :topline line)
    (vim.fn.winrestview)))


(defn- folds-in-range [start end folds]
  "Return the folds that overlap with the given range, inclusive."
  (var out [])
  (var end-found? nil)
  (each [_ {:start fstart :end fend} (ipairs folds) &until end-found?]
    (when (or (<= fstart start end fend) ; fold engulfs range
              (<= start fstart fend end) ; fold within range
              (<= fstart start fend end) ; range starts within fold
              (<= start fstart end fend)) ; fold starts within range
      (table.insert out {:start fstart :end fend})
      (when (<= end fend)
        (set end-found? true))))
  out)

(comment
  (folds-in-range 10 12 [{:start 6 :end 13}])
  (folds-in-range 7 13 [{:start 6 :end 12}])
  (folds-in-range 7 13 [{:start 7 :end 12}])
  (folds-in-range 7 20 [{:start 6 :end 12} {:start 15 :end 22}])
  (folds-in-range 7 20 [{:start 6 :end 12} {:start 15 :end 18}])
  (folds-in-range 7 20 [{:start 15 :end 18}])
  (folds-in-range 20 40 [{:start 6 :end 12} {:start 15 :end 18}])
  (folds-in-range 20 40 [{:start 6 :end 12} {:start 42 :end 44}]))

(defn- count-hidden-lines [start end folds]
  "Count the number of lines that are hidden by folds in the given range, 
  not counting folds at the start of the range"
  (->> (folds-in-range start end folds)
       (r.map (fn [{:start fstart :end fend}]
                (- fend fstart)))
       (r.reduce r.add 0)))

(comment
  (count-hidden-lines 10 12 [{:start 6 :end 13}]) ; 7
  (count-hidden-lines 7 13 [{:start 6 :end 12}]) ; 6
  (count-hidden-lines 7 20 [{:start 6 :end 12} {:start 15 :end 22}]) ; 13
  (count-hidden-lines 7 20 [{:start 6 :end 12} {:start 15 :end 18}]) ; 9
  (count-hidden-lines 20 40 [{:start 6 :end 12} {:start 15 :end 18}])
  (count-hidden-lines 20 40 [{:start 6 :end 12} {:start 42 :end 44}]))

(defn calc-topline [{: folds 
                     : top-margin 
                     : cur-lnum}]
  "Calculate the topline of the current window.
  In essence, this function will scroll the window so that the
  current line is always at the same level in the window.

  The algorithm is as follows:

  0. The upper margin is set to 60% of the window height in lines.
  1. If the current bufferline is below the upper margin, then the topline is 
     set to the current bufferline minus the upper margin.
  3. Folds count as one line, so the lines within a fold are subtracted from
     the topline
  "
  (let [; topline without folds
        next-topline (->
                       cur-lnum
                       (- top-margin)
                       (+ 1) ;; 1-based line numbers
                       (math.max 1)) ;; don't go below 1

          
        hidden-lines (count-hidden-lines next-topline cur-lnum folds)
        _ (a.println :topline next-topline :hidden hidden-lines)

        ; subtract hidden lines from topline
        ; if no hidden lines, next-topline stays the same
        next-topline (-> next-topline
                         (- hidden-lines)
                         (math.max 1))
        _ (a.println :topline next-topline)
        next-topline (if-let [fold (fld.in-fold? next-topline folds)]
                       (if (= next-topline fold.start)
                         fold.start
                         (+ fold.end 1))
                       next-topline)]
          
    next-topline))

(comment
  (calc-topline {:folds []
                 :top-margin 10
                 :cur-lnum 20}) ; 11
  (calc-topline {:folds []
                 :top-margin 40
                 :cur-lnum 40}) ; 1
  (calc-topline {:folds []
                 :top-margin 40
                 :cur-lnum 41}) ; 2
  (calc-topline {:folds [{:start 10 :end 12}] ; two hidden lines
                 :top-margin 40
                 :cur-lnum 43}) ; 2

  ; topline approaching fold
  (calc-topline {:folds [{:start 21 :end 23}] 
                 :top-margin 20
                 :cur-lnum 39})  ; 18
  (calc-topline {:folds [{:start 21 :end 23}] 
                 :top-margin 20
                 :cur-lnum 40})  ; 19
  (calc-topline {:folds [{:start 21 :end 23}] 
                 :top-margin 20
                 :cur-lnum 41})  ; 20
  (calc-topline {:folds [{:start 21 :end 23}] 
                 :top-margin 20
                 :cur-lnum 42})  ; 21
  (calc-topline {:folds [{:start 21 :end 23}] 
                 :top-margin 20
                 :cur-lnum 43})  ; 24
  (calc-topline {:folds [{:start 21 :end 23}] 
                 :top-margin 20
                 :cur-lnum 44})  ; 25

  ; topline approaching fold and fold within range (4 hidden lines)
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 18
                 :cur-lnum 40})  ; 19
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 18
                 :cur-lnum 41})  ; 20
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 42})  ; 21
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 43})  ; 24
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 44})  ; 25
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 45}))  ; 26
