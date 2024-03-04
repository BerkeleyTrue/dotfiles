(module lib.scroll-fix
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def- configs
  {:margin 60
   :is_enabled? true
   :fix_at_eof true
   :debug false})

(defn- set-top-of-window [line wintable]
  "set the top line of the window to be `line`
   this effectively scrolls the window"
  (-> wintable
    (a.assoc :topline line)
    (vim.fn.winrestview)))

(defn- get-world-facts []
  "get facts about the current buffer, window, and cursor"
  (let [winheight (vim.fn.winheight 0) ; current window viewport height
        wintable (vim.fn.winsaveview) ;
        current-cursor-win-line (. wintable :lnum)
        top-visible-line (. wintable :topline)
        buf-last-line (vim.fn.line "$")]
    {:winheight winheight
     :wintable wintable
     :current-cursor-win-line current-cursor-win-line
     :top-visible-line top-visible-line
     :buf-last-line buf-last-line}))

(comment (get-world-facts))

(defn scroll-fix []
  "Scroll fix - pull the current state of the window, buffer, and cursor
   and calculate whether to adjust the window position in order to keep the cursor
   at the center of the screen.
   TODO: figure out how to get fold info
  "
  (let [
        ;; configs
        fix-percent (a.get configs :margin 60) ; how far down the window should the cursor be fixed at
        is-enabled? (a.get configs :is_enabled? true) ; whether to enable the plugin
        fix-at-eof (a.get configs :fix_at_eof true) ; whether to fix the cursor at the end of the file
        is-debug? (a.get configs :debug false) ; whether to print debug info

        ;; world facts
        {: winheight : wintable : current-cursor-win-line : top-visible-line : buf-last-line} (get-world-facts)
        ;; derived

        ;; get the number of lines (margin) above the cursor
        ;; use the window height
        ;; multiply by fix-percent (defaults to 60 percent of window)
        ;; get percent (div 100)
        ;; round to int
        desired-win-margin (-> winheight
                             (* fix-percent)
                             (/ 100)
                             (math.floor))

        ;; get the line number of the cursor in the buffer that we want to fix at
        desired-buf-line (+ top-visible-line desired-win-margin)

        ;; are we at the beginning of the buffer and the top of the window?
        is-at-beg-of-buff? (and
                             (= top-visible-line 1)
                             (<= current-cursor-win-line (- desired-buf-line 1)))
        is-on-desired? (=
                         (+ (- current-cursor-win-line top-visible-line) 1)
                         desired-win-margin)

        is-below-desired? (>= current-cursor-win-line desired-win-margin)
        is-eof? (> (+ winheight top-visible-line)
                   buf-last-line)]

    (when is-debug?
      (a.pr "current-buf-line: " current-cursor-win-line
            "desired-buf-line: " desired-buf-line
            "is-above-buf-margin?: " is-at-beg-of-buff?
            "is-on-desired?: " is-on-desired?
            "is-below-desired?: " is-below-desired?
            "is-eof?: " is-eof?))

    (when is-enabled?

      ;; make sure softtabstop is off
      ;; not sure why this is needed
      (when (not= (a.get vim.o :softtabstop) 0)
        (set vim.o.softtabstop 0))

      (when-not
        (or
          is-at-beg-of-buff?
          is-on-desired?
          (and
            (not fix-at-eof)
            is-below-desired?
            is-eof?))

        (set-top-of-window
          (->
            current-cursor-win-line
            (- desired-win-margin)
            (+ 1))
          wintable)))))

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
      (let [to-enable (not (a.get configs :is_enabled? true))]
        (a.assoc configs :is_enabled? to-enable)
        (when to-enable (scroll-fix))))))
