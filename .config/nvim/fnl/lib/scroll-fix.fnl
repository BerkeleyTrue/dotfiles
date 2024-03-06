(module lib.scroll-fix
  {autoload
   {a aniseed.core
    r r
    fld lib.folds}
   require-macros [macros]})

(def- configs
  {:margin 60
   :enabled? true
   :debug false})

(defn- set-top-of-window [line wintable]
  "set the top line of the window to be `line`
   this effectively scrolls the window"
  (-> wintable
    (a.assoc :topline line)
    (vim.fn.winrestview)))

(defn- get-world-facts []
  "get facts about the current buffer, window, and cursor"
  (let [winheight     (vf winheight 0) ; current window viewport height
        wintable      (vf winsaveview) ;
        buf-last-line (vf line "$")
        hidden-lines  (fld.count-folded-lines wintable.lnum)
        lnum wintable.lnum
        top-visible-line        wintable.topline]

    {: winheight
     : wintable
     : lnum
     : top-visible-line
     : buf-last-line
     : hidden-lines}))

(comment (get-world-facts))

(defn scroll-fix []
  "Scroll fix - pull the current state of the window, buffer, and cursor
   and calculate whether to adjust the window position in order to keep the cursor
   at the center of the screen.
   TODO: topline needs to be adjusted when in fold
  "
  (let [
        ;; configs
        fix-percent configs.margin ; how far down the window should the cursor be fixed at
        is-enabled? (a.get configs :enabled? true) ; whether to enable the plugin
        debug? configs.debug ; whether to print debug info

        ;; world facts
        {: winheight 
         : wintable 
         : lnum 
         : top-visible-line 
         : buf-last-line
         : hidden-lines} (get-world-facts)
        ;; derived
        ;; get the bufer line number of the cursor taking hidden lines into account
        lnum* (- lnum hidden-lines)

        ;; get the number of lines (margin) above the cursor
        ;; use the window height
        ;; multiply by fix-percent (defaults to 60 percent of window)
        ;; get percent (div 100)
        ;; round to int
        desired-win-margin (-> winheight
                             (* fix-percent)
                             (/ 100)
                             (math.floor))

        ;; get the desired number of lines above the cursor in the view
        desired-buf-lac (+ top-visible-line desired-win-margin)

        ;; are we at the beginning of the buffer and the top of the window?
        is-at-beg-of-buff? (and (= top-visible-line 1)
                                (<= lnum* (- desired-buf-lac 1)))
        is-on-desired? (= desired-win-margin
                          (->
                            lnum* 
                            (- top-visible-line)
                            (+ 1)))

        is-below-desired? (>= lnum* desired-win-margin)
        is-eof? (> (+ winheight top-visible-line)
                   buf-last-line)
        desired-top-line (->
                           lnum*
                           (- desired-win-margin)
                           (+ 1))]

    (when debug?
      (a.pr "lnum" lnum
            "hidden-lines" hidden-lines
            "lnum*" lnum*
            "desired-buf-lac" desired-buf-lac
            "top-line" top-visible-line
            "desired-top-line" desired-top-line
            "is-above-buf-margin?" is-at-beg-of-buff?
            "is-on-desired?" is-on-desired?
            "is-below-desired?" is-below-desired?
            "is-eof?" is-eof?))

    (when (and is-enabled?
               (not (or is-at-beg-of-buff?
                        is-on-desired?)))
      (set-top-of-window desired-top-line wintable))))

(defn main []
  (augroup
    :ScrollFixGroup
    {:event [:CursorMoved 
             :CursorMovedI 
             :BufEnter 
             :BufFilePre 
             :BufWritePost 
             :VimResized
             :VimResume]
     :pattern :*
     :callback scroll-fix})

  (nnoremap :zz scroll-fix)

  (command!
    :ScrollFixToggle
    (fn scroll-fix-toggle []
      (let [to-enable (not (a.get configs :enabled? true))]
        (a.assoc configs :enabled? to-enable)
        (when to-enable (scroll-fix))))))
