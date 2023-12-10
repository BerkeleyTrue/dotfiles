(module lib.autoresizer
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def- golden-ratio 1.618)

(defn- goldenize [width]
  "Get the golden ratio given a width."
  (->
    width
    (/ golden-ratio)
    math.floor))

(defn- get-windows []
  "Get all the non-modifiable windows."
  (->>
    (vim.fn.winnr "$")
    (r.range 1)
    (r.filter #(= (vim.fn.getwinvar $ "&modifiable") 1))))

(comment
  (get-windows))

(defn- get-parallel-windows [cur-win]
  "get the windows that are parallel to the current window"
  (let [cur-height (vim.fn.winheight cur-win)]
    (->>
      (get-windows)
      (r.filter
        (fn [w]
          (and
            (not= w cur-win)
            (= (vim.fn.winheight w) cur-height)))))))
(comment
  (get-parallel-windows 2))
