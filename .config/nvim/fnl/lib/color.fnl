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
  (->hsl "#66eb00"))

(defn lit [[hue sat] lit]
  [hue sat lit])
