(module dotfiles.plugins.scroll-fix
  {:require {a aniseed.core
             nvim aniseed.nvim
             bridge dotfiles.bridge}})


(defn- set-top-of-window [line wintable]
  ;; set the top line of the window to be `line`
  ;; this effectively scrolls the winodw
  (-> wintable
    (a.assoc :topline line)
    (nvim.fn.winrestview)))

(defn print-info [desired-win-line fix-percent winheight]
  ;; prints info when the window height changes
  (when (or (not (nvim.fn.exists "b:desired_win_line"))
            (not= (. nvim.b :desired_win_line) desired-win-line))

    (set nvim.b.desired_win_line desired-win-line)

    (let [saved-lzy-redraw-option (. nvim.o :lazyredraw)]
      ;; for redraw
      (set nvim.o.lazyredraw false)
      (nvim.ex.redraw)

      ;; echo info
      (nvim.echo (..
                   "Scroll fixed at line " (. nvim.b :desired_win_line)
                   "/" winheight
                   " (" fix-percent "%)"))

      ;; revert lazyredraw settings
      (set nvim.o.lazyredraw saved-lzy-redraw-option))))


(defn scroll-fix []
  ;; Scroll fix - pull the current state of the window, buffer, and cursor
  ;; and calculate whether to adjust the window position in order to keep the cursor
  ;; at the center of the screen
  (let [
        ;; configs
        fix-percent (a.get nvim.g :scroll_fix_percent 60)
        is-enabled? (a.get nvim.g :scroll_fix_enabled true)
        fix-at-eof (a.get nvim.g :scroll_fix_at-eof true)
        is-debug (a.get nvim.g :scroll_fix_debug false)

        ;; world facts
        winheight (nvim.fn.winheight 0)
        wintable (nvim.fn.winsaveview)
        current-buf-line (. wintable :lnum)
        top-visible-line (. wintable :topline)
        buf-last-line (nvim.fn.line "$")
        ;; derived

        ;; get the window height
        ;; multiply by fix-percent (defaults to 60 percent of window)
        ;; get percent (div 100)
        desired-win-line (/
                          (* winheight fix-percent)
                          100)

        desired-buf-line (+ top-visible-line desired-win-line)

        ;; when the current buffer line
        ;; is less than the desired window line
        ;; it is at the beginning of the buffer
        is-above-buf-margin? (<= current-buf-line desired-win-line)
        is-on-desired? (=
                        (+ (- current-buf-line top-visible-line) 1)
                        desired-win-line)

        is-below-desired? (>= current-buf-line desired-win-line)
        is-eof? (> (+ winheight top-visible-line)
                   buf-last-line)]

    (when is-debug?
      (print "current-buf-line: " current-buf-line)
      (print "desired-buf-line: " desired-buf-line)
      (print "is-above-buf-margin?: " is-above-buf-margin?)
      (print "is-on-desired?: " is-on-desired?)
      (print "is-below-desired?: " is-below-desired?)
      (print "is-eof?: " is-eof?))

    (when is-enabled?

      ;; make sure softtabstop is off
      ;; not sure why this is needed
      (when (not= (a.get nvim.o :softtabstop) 0)
        (set nvim.o.softtabstop 0))

      (when (not (or
                   is-above-buf-margin?
                   is-on-desired?
                   (and fix-at-eof
                        is-eof?
                        is-below-desired?)))


        (print-info desired-win-line fix-percent winheight)
        (set-top-of-window (+
                            (- current-buf-line desired-win-line)
                            1))))))

(do
  (nvim.ex.augroup :scroll_fix_au)
  (nvim.ex.autocmd_)
  (nvim.ex.autocmd (..
                     "CursorMoved,CursorMovedI * "
                     ":"
                     (bridge.viml->lua
                       :dotfiles.plugins.scroll-fix
                       :scroll-fix)))

  (nvim.ex.augroup :END)
  {:scroll-fix scroll-fix})
