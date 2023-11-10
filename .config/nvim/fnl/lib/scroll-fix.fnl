(module lib.scroll-fix
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(defn- set-top-of-window [line wintable]
  ;; set the top line of the window to be `line`
  ;; this effectively scrolls the window
  (-> wintable
    (a.assoc :topline line)
    (vim.fn.winrestview)))

(defn- print-info [desired-win-line fix-percent winheight]
  ;; prints info when the window height changes
  (when (or (not (vim.fn.exists "b:desired_win_line"))
            (not= (. vim.b :desired_win_line) desired-win-line))

    (set vim.b.desired_win_line desired-win-line)

    (let [saved-lzy-redraw-option (. vim.o :lazyredraw)]
      ;; for redraw
      (set vim.o.lazyredraw false)
      ; sometimes this will cause `no buffer for <n> errors to happen silently`
      (command redraw)

      ;; echo info
      (comment (vim.echo (..
                           "Scroll fixed at line " (a.pr-str (. vim.b :desired_win_line))
                           "/" winheight
                           " (" fix-percent "%)")))

      ;; revert lazyredraw settings
      (set vim.o.lazyredraw saved-lzy-redraw-option))))


(defn scroll-fix []
  "Scroll fix - pull the current state of the window, buffer, and cursor
   and calculate whether to adjust the window position in order to keep the cursor
   at the center of the screen.
   TODO: figure out how to get fold info
  "
  (let [
        ;; configs
        fix-percent (a.get vim.g :scroll_fix_percent 60) ; how far down the window should the cursor be fixed at
        is-enabled? (a.get vim.g :scroll_fix_enabled true) ; whether to enable the plugin
        fix-at-eof (a.get vim.g :scroll_fix_at_eof true) ; whether to fix the cursor at the end of the file
        is-debug? (a.get vim.g :scroll_fix_debug false) ; whether to print debug info

        ;; world facts
        winheight (vim.fn.winheight 0)
        wintable (vim.fn.winsaveview)
        current-buf-line (. wintable :lnum)
        top-visible-line (. wintable :topline)
        buf-last-line (vim.fn.line "$")
        fold-close-line (vim.fn.foldclosedend ".")
        is-on-fold (not= fold-close-line -1) ; whether the cursor is on a fold, currently we do nothing with this
        ;; derived

        ;; get the window height
        ;; multiply by fix-percent (defaults to 60 percent of window)
        ;; get percent (div 100)
        ;; round to int
        desired-win-line (-> winheight
                             (* fix-percent)
                             (/ 100)
                             (math.floor))

        desired-buf-line (+ top-visible-line desired-win-line)

        ;; when the current buffer line
        ;; is less than the desired window line
        ;; it is at the beginning of the buffer
        is-above-buf-margin? (<= current-buf-line (- desired-win-line 1))
        is-on-desired? (=
                        (+ (- current-buf-line top-visible-line) 1)
                        desired-win-line)

        is-below-desired? (>= current-buf-line desired-win-line)
        is-eof? (> (+ winheight top-visible-line)
                   buf-last-line)]

    (when is-debug?
      (print (..
               "current-buf-line: " current-buf-line
               "\ndesired-buf-line: " desired-buf-line
               "\nis-above-buf-margin?: " is-above-buf-margin?
               "\nis-on-desired?: " is-on-desired?
               "\nis-below-desired?: " is-below-desired?
               "\nis-eof?: " is-eof?)))

    (when is-enabled?

      ;; make sure softtabstop is off
      ;; not sure why this is needed
      (when (not= (a.get vim.o :softtabstop) 0)
        (set vim.o.softtabstop 0))

      (when (not (or
                   is-above-buf-margin?
                   is-on-desired?
                   (and (not fix-at-eof)
                        is-eof?
                        is-below-desired?)))


        (print-info desired-win-line fix-percent winheight)
        (set-top-of-window (+
                            (- current-buf-line desired-win-line)
                            1))))))

(defn main []
  (augroup
    :ScrollFixGroup
    {:event [:CursorMoved :CursorMovedI :BufEnter :BufFilePre]
     :pattern :*
     :callback scroll-fix})
  (nnoremap :zz scroll-fix))
