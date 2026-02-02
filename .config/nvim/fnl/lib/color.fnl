(module lib.color
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

; see https://homepages.cwi.nl/~steven/css/hsl.html
(defn- hue->rgb [m1 m2 hue]
  (let [hue (if
              (< hue 0) (+ hue 1)
              (> hue 1) (- hue 1)
              hue)]
    (if
      (< hue (/ 1 6)) (+ m1 (* (- m2 m1) hue 6))
      (< hue (/ 1 2)) m2
      (< hue (/ 2 3)) (+ m1 (* (- m2 m1) (- (/ 2 3) hue) 6))
      m1)))

(defn- hsl->rgb [hue sat lit]
  "Converts a hex string to a table of Hue Saturation Lightness values.
  Assume h, s, l are numbers from 0 to 1
  returns a set of r g b values from 0 to 255"
  (if (= sat 0)
    (let [lit (r.round (* lit 255) 0)]
      (values lit lit lit))
    (let [m2 (if (< lit 0.5)
               (* lit (+ sat 1))
               (+ lit (- sat (* lit sat))))
          m1 (- (* lit 2) m2)
          _r (hue->rgb m1 m2 (+ hue (/ 1 3)))
          _g (hue->rgb m1 m2 hue)
          _b (hue->rgb m1 m2 (- hue (/ 1 3)))]
      (values (* _r 255) (* _g 255) (* _b 255)))))


(comment
  (hsl->rgb 0 1 0.5) ; red
  (hsl->rgb (/ 120 360) 1 0.5) ; green
  (hsl->rgb (/ 240 360) 1 0.5)) ; blue

(defn ->hex [[hue sat lit]]
  (let [(red grn bl) (hsl->rgb (/ hue 360) (/ sat 100) (/ lit 100))]
    (string.format "#%02x%02x%02x" red grn bl)))

(comment
  (->hex [0 100 50]) ; red
  (->hex [120 100 50]) ; green
  (->hex [240 100 50])) ; blue

(defn rgb->hsl [red grn bl]
  "Converts a set of r g b values from 0 to 1 to a set of Hue Saturation Lightness values."
  (let [max (r.clamp 0 1 (math.max red grn bl))
        min (r.clamp 0 1 (math.min red grn bl))
        delta (- max min)
        lit (/ (+ max min) 2)]
    (if (= min max)
      ; Achromatic, can skip the rest
      (values 0 0 lit)
      (let [sat (if (< lit 0.5)
                  (/ delta (+ max min))
                  (/ delta (- 2 max min)))
            hue (if
                  (<= delta 0) 0
                  (= max red) (% (/ (- grn bl) delta) 6)
                  (= max grn) (+ (/ (- bl red) delta) 2)
                  (+ (/ (- red grn) delta) 4))
            hue (/ hue 6)
            hue (if (< hue 0) (+ hue 1) hue)
            hue (if (> hue 1) (- hue 1) hue)]
        (values (r.round (* hue 360) 0) (r.round (* sat 100) 0) (r.round (* lit 100) 0))))))

(comment
  (rgb->hsl 1 0 0)
  (rgb->hsl 0 1 0)
  (rgb->hsl 0 0 1)
  (rgb->hsl (/ 72 255) 1 0)
  (rgb->hsl (/ 102 255) (/ 235 255) 0)
  (rgb->hsl (/ 240 255) (/ 105 255) 0))

(defn ->hsl [hex]
  (let [red (string.sub hex 2 3)
        grn (string.sub hex 4 5)
        bl (string.sub hex 6 7)]
    (rgb->hsl (/ (tonumber red 16) 255) (/ (tonumber grn 16) 255) (/ (tonumber bl 16) 255))))

(comment
  (->hsl "#ff0000")
  (->hsl "#00ff00")
  (->hsl "#0000ff")
  (->hsl "#48eb00")
  (->hsl "#66eb00")
  (->hsl "#f4b8e4"))

(defn lit [[hue sat] lit]
  [hue sat lit])

(defn darken [[hue sat lit] amount]
  (let [amount (or amount 10)
        new-lit (r.clamp 0 100 (- lit amount))]
    [hue sat new-lit]))

(comment 
  (darken [100 100 50] 10)
  (->hex (darken [(->hsl "#f4b8e4")])))

(defn darken-hex [hex]
  (-> hex
      (->hsl)
      (#[$1 $2 $3])
      (darken)
      (->hex)))

(comment
  (darken-hex "#f4b8e4")) ; "#ec87d1"

(defn brighten [[hue sat lit] amount]
  (let [amount (or amount 10)
        new-lit (r.clamp 0 100 (+ lit amount))]
    [hue sat new-lit]))

(comment 
  (brighten [100 100 50] 10)
  (->hex (brighten [(->hsl "#f4b8e4")]))) ; "#fae0f3"

(defn brighten-hex [hex]
  (-> hex
      (->hsl)
      (#[$1 $2 $3])
      (brighten)
      (->hex)))

(comment
  (brighten-hex "#f4b8e4"))  ; "#fae0f3"

(defn- get-visual-selection []
  "Get the current visual selection text"
  (let [[_ start-row start-col] (vim.fn.getpos "'<")
        [_ end-row end-col] (vim.fn.getpos "'>")
        lines (vim.api.nvim_buf_get_text 0 (- start-row 1) (- start-col 1) (- end-row 1) end-col {})]
    (table.concat lines "\n")))

(defn- replace-visual-selection [new-text]
  "Replace the current visual selection with new-text"
  (let [[_ start-row start-col] (vim.fn.getpos "'<")
        [_ end-row end-col] (vim.fn.getpos "'>")]
    (vim.api.nvim_buf_set_text 0 (- start-row 1) (- start-col 1) (- end-row 1) end-col [new-text])))

(defn- gen-new-hex [hex transform-fn]
  (-> (.. "#" hex)
      (->hsl)
      (#[$1 $2 $3])
      (transform-fn)
      (->hex)
      (string.sub 2)))

(defn- transform-hex [transform-fn amount]
  "Transform hex color under cursor or in visual selection"
  (if-let [hex (get-visual-selection)]
    (-> hex
        (gen-new-hex #(transform-fn $ (or amount 10)))
        (replace-visual-selection hex))
    (vim.notify "lib.color: No hex color found" vim.log.levels.WARN)))

(defn brighten-cmd [opts]
  "Command handler for brightening hex colors"
  (let [amount (if (and opts.args (not= opts.args ""))
                 (tonumber opts.args)
                 10)
        is-range (= opts.range 2)]
    (if is-range
      (transform-hex brighten amount)
      (vim.notify "lib.color: Visual mode only" vim.log.levels.WARN))))

(defn darken-cmd [opts]
  "Command handler for darkening hex colors"
  (let [amount (if (and opts.args 
                        (not= opts.args ""))
                 (tonumber opts.args)
                 10)
        is-range (= opts.range 2)]
    (if is-range
      (transform-hex darken amount)
      (vim.notify "lib.color: Visual mode only" vim.log.levels.WARN))))

(command! :BrightenHex brighten-cmd {:nargs "?" :range true :desc "Brighten hex color under cursor"})
(command! :DarkenHex darken-cmd {:nargs "?" :range true :desc "Darken hex color under cursor"})

(comment
  "#191919")
