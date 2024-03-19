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

(defn fix-topline []
  "Fix the topline of the current window.
  In essence, this function will scroll the window so that the
  current line is always at the same level in the window.

  The algorithm is as follows:

  0. The upper margin is set to 60% of the window height in lines.
  1. If the current bufferline is below the upper margin, then the topline is 
     set to the current bufferline minus the upper margin.
  4. If the calculated topline lands within a fold the topline is set to the last line of the fold. 
     5. If adding the top margin to the new topline is greater than the current line number,
        then the topline is set to the line before the fold
  3. If there are any folds within the upper margin, then the number of lines in the fold
      are added to the upper margin to calculate the topline.
  "
  (when enabled
    (let [winheight (vf winheight 0)
          wintable  (vf winsaveview)
          folds (fld.do-fold-collection)

          cur-lnum    wintable.lnum

          top-margin (-> winheight
                         (* 60)
                         (/ 100)
                         (math.floor))

          next-topline (->
                         cur-lnum
                         (- top-margin)
                         (+ 1) ;; 1-based line numbers
                         (math.max 1)) ;; don't go below 1

          ;; if the next topline is within a fold, then set it to the start of the fold
          next-topline*  (if-let [fold (fld.in-fold? next-topline folds)]
                           fold.end
                           next-topline)
          
          hidden-lines (fld.count-folded-lines {:start next-topline*
                                                :end cur-lnum}
                                               folds)]
          
      "")))
