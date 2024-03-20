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

(defn calc-topline [{: folds 
                     : top-margin 
                     : cur-lnum}]
  "Calculate the topline of the current window.
  In essence, this function will scroll the window so that the
  current line is always at the same level in the window.
  "
  ; topline is set to the current line
  ; it is then iteratively reduced by the folds until folds are exhuasted
  ; or the remaining margin is less than 0
  (var topline cur-lnum)
  (var remaining-margin (- top-margin 1))

  ; iterate through each fold, if the end of the fold is within top-margin
  (each [_ fold (ipairs (r.reverse folds)) &until (<= remaining-margin 0)]
    ; folds below the current topline are not considered
    ; folds above the current topline are considered
    ; if the distance between the topline and the fold end is less than the remaining margin
    ; then the topline is set to the start of the fold
    ; the remaining margin is then reduced by the number of lines between the current topline and the end of the fold
    ; else the remaining margin does not reach the fold and the topline is set to the difference with the remaining margin
    (when (< fold.end topline)
      (a.println :fold fold 
                 :topline topline 
                 :remaining-margin remaining-margin 
                 :lines-to-fold (- topline fold.end))
      (if (>= remaining-margin (- topline fold.end))
        (do
          (a.println "setting topline to fold start" fold.start)
          (set remaining-margin (- remaining-margin (- topline fold.end))) 
          (set topline fold.start))

        (do
          (a.println "setting topline to remaining margin" (- topline remaining-margin))
          (set topline (- topline remaining-margin))
          (set remaining-margin 0)))))

  (a.println :topline topline :remaining-margin remaining-margin)
  (-> topline
    (- (math.max remaining-margin 0))
    (math.max 1)))
           

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
                 :top-margin 20
                 :cur-lnum 40})  ; 17
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 41})  ; 18
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 42})  ; 19
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 43})  ; 20
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 44})  ; 21
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 45})  ; 24
  (calc-topline {:folds [{:start 21 :end 23} {:start 35 :end 37}] 
                 :top-margin 20
                 :cur-lnum 60}))  ; 
