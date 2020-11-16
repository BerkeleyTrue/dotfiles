(module plugins.woating
  {:require {a aniseed.core
             nvim aniseed.nvim}})

(defn create-buffer []
  (let [buf (nvim.create_buf false true)]
    (nvim.buf_set_option buf :buftype "nofile")
    buf))

(defn create-woating []
  (let [width (. nvim.o :columns)
        height (. nvim.o :lines)
        is-wide-enough (> width 150)

        buf (create-buffer)

        win-width (if is-wide-enough
                    (-> width
                        (* 0.9)
                        (math.ceil))
                    (- width 8))

        win-height (-> height
                       (* 3)
                       (/ 4)
                       (math.ceil)
                       (math.min 30))

        row (-> height
                (- win-height)
                (/ 2)
                (math.ceil))

        col (-> width
                (- win-width)
                (/ 2)
                (math.ceil))

        opts {:relative "editor"
              :width win-width
              :height win-height
              :row row
              :col col}]

      (nvim.open_win buf true opts)
      buf))
