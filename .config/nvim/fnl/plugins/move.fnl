(module plugins.move
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})


(def- moveKeyModifier :A)
(var lastChangedTick -1)
(var lastDir nil)

(defn- create-move-key [key]
  (.. "<" moveKeyModifier "-" key ">"))

(comment (create-move-key :j))

(defn- get-half-page-size [] (/ (vf winheight :.) 2))

(defn- undo-join [dir]
  (let [last lastDir
        unchanged? (= lastChangedTick (. utils.b :changedtick))
        same-dir? (= lastDir dir)]
    (when (and unchanged? same-dir?)
      (utils.ex.silent_ :undojoin))))

(defn- save-move-info [dir]
  (set lastDir dir)
  (set lastChangedTick (. utils.b :changedtick)))

(defn- execute-move-vert [line distance]
  "Compute the destination line. Instead of simply incrementing the line
  number, we move the cursor with `j` and `k`. This ensures that the
  destination line is in bounds and it also goes past closed folds."
  (let [down? (> distance 0)
        key (if up? "k" "j")]
    (vf cursor line 1)
    (cmd
      normal!
      {:args
       [(if down?
          (.. distance "j")
          (.. (- distance) "k"))]})
    (if
      (not down?) (- (vf line ".") 1)
      (let [fold-closed-end (vf foldclosedend ".")]
        (if (= fold-closed-end -1)
          (vf line ".")
          fold-closed-end)))))

(defn- move-vertically [first last distance]
  "Move and reindent given lines down (distance > 0) or up (distance < 0)"
  (when
    (and
      (. utils.o :modifiable)
      (not= distance 0))
    (let [down? (> distance 0)
          first (vf line first)
          last (vf line last)
          old-pos (vf getcurpos)
          after (execute-move-vert (if down? first last) distance)]

      ; Restoring the cursor position might seem redundant because of the
      ; upcoming :move. However, it prevents a weird issue where undoing a move
      ; across a folded section causes it to unfold.
      (vf setpos "." old-pos)

      (undo-join (if down? "down" "up"))
      (cmd move {:range [first last] :args [after]})

      (let [first (vf line "'[")
            last (vf line "']")]
        (vf cursor first 1)

        (let [old-indent (vf indent ".")]
          (command normal! "==")
          (let [new-indent (vf indent ".")]
            (when (and (< first last) (not= old-indent new-indent))
              (let [op (if (< old-indent new-indent)
                         (vf repeat ">" (- new-indent old-indent))
                         (vf repeat "<" (- old-indent new-indent)))
                    old-sw (o shiftwidth)]
                (o! shiftwidth 1)
                (cmd op {:range [(+ first 1) last]})
                (o! shiftwidth old-sw)))
            (vf cursor first 1)
            (command normal! "0m[")
            (vf cursor last 1)
            (command normal! "$m]")
            (save-move-info (< distance 0) "up" "down")))))))

(defn- move-line-vertically [distance]
  (let [old-col (vf col ".")]
    (command "normal! ^")
    (let [old-indent (vf col ".")]
      (move-vertically "." "." distance))))

(defn- move-block-vertically [distance])

(defn- move-horizontally [cornerStart cornerEnd distance])
(defn- move-char-horizontally [distance])
(defn- move-block-horizontally [distance])

(defn move-block-down [] (move-block-vertically (v count1)))
(defn move-block-up [] (move-block-vertically (- (v count1))))
(defn move-block-right [] (move-block-horizontally (v count1)))
(defn move-block-left [] (move-block-horizontally (- (v count1))))

(defn move-line-down [] (move-line-vertically (v count1)))
(defn move-line-up [] (move-line-vertically (- (v count1))))
(defn moveLineRight [] (move-block-horizontally (v count1)))
(defn moveLineLeft [] (move-block-horizontally (- (v count1))))

(defn move-char-right [] (move-char-horizontally (v count1)))
(defn move-char-left [] (move-char-horizontally (v count1)))

(defn main []
  (utils.vnoremap (create-move-key :j) (cviml->lua* move-block-down) {:silent true})
  (utils.vnoremap (create-move-key :k) (cviml->lua* move-block-up) {:silent true})
  (utils.vnoremap (create-move-key :h) (cviml->lua* move-block-left) {:silent true})
  (utils.vnoremap (create-move-key :l) (cviml->lua* move-block-right) {:silent true})

  (utils.nnoremap (create-move-key :j) (cviml->lua* move-line-down) {:silent true})
  (utils.nnoremap (create-move-key :k) (cviml->lua* move-line-up) {:silent true})
  (utils.nnoremap (create-move-key :h) (cviml->lua* move-char-left) {:silent true})
  (utils.nnoremap (create-move-key :l) (cviml->lua* move-char-right) {:silent true}))
