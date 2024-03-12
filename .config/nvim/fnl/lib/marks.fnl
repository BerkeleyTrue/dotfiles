(module lib.marks
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(comment
  (vf getmarklist) ; global
  (vf getmarklist "%")) ; buffer local
  ; mark pos [bufnr lnum col offset]

(def- group "LibMarks")

(def- builtins 
  (r.reduce
    #(r.assoc $1 $2 true)
    {}
    [:. :^ "`" "'" "\"" :< :> "[" "]" 
     0 1 2 3 4 5 6 7 8 9]))

(def- shown-builtins
  [:< :>])

;; >==<buf cache>==<
(var buf-cache {})

(defn- buf->cached [bufnr]
  (when-not (. buf-cache bufnr)
    (tset buf-cache bufnr {:placed {} 
                           :by-line {}}))
  (. buf-cache bufnr))

(comment
  (buf->cached (n get-current-buf)))

(defn- buf->nil [bufnr]
  (r.assoc! buf-cache bufnr nil))

(comment
  (buf->nil (n get-current-buf)))

;; >==<raw signs>==<
(defn create-sign [bufnr text lnum id priority]
  (let [priority (or priority 10)
        name (.. group "-" text)]
    (when-not (. builtins name)
      (tset builtins name true)
      (vf sign-define name
          {: text
           :texthl "MarksSignHL"
           :numhl "MarksSignNumHL"}))
    (vf sign-place id group name bufnr {: lnum : priority})))

(defn delete-sign [bufnr id]
  (vf sign-unplace group {:id id : bufnr}))

(defn remove-all-signs [bufnr]
  (vf sign-unplace group {: bufnr}))

(comment 
  (do
    (remove-all-signs (n get-current-buf))
    (buf->nil (n get-current-buf))))

;; >==<marks>==<
(defn delete-mark [mark bufnr clear]
  (let [clear (or clear true)
        bufnr (or bufnr (n get-current-buf))
        cached (buf->cached bufnr)
        placed (. cached :placed)
        by-line (. cached :by-line)]
    (when-let [data (. placed mark)]
      (delete-sign bufnr data.id)
      (r.update-in! cached [:by-line data.line] #(r.reject (r.is-equal mark) $))
      (r.assoc-in! cached [:placed mark] nil)
      (when clear (command delmark mark)))))

(comment (delete-mark "y"))

(defn register-mark [mark line col bufnr]
  (let [bufnr (or bufnr (n get-current-buf))
        cached (buf->cached bufnr)]

    (when (r.get-in cached [:placed mark])
      (delete-mark mark))

    (if (r.get-in cached [:by-line line])
      (r.update-in! cached [:by-line line] #(->>
                                              $
                                              (r.concat [mark])
                                              (r.uniq)))
      (r.assoc-in! cached [:by-line line] [mark]))

    (let [id (* (string.byte mark) 100)
          text (->>
                 (r.get-in cached [:by-line line])
                 (r.take-last 2)
                 (r.join ""))]
      (r.assoc-in! cached [:placed mark] {: id : line : col}) 
      (create-sign bufnr text line id))))

(comment
  (register-mark "z" 70 4))

(defn refresh-marks [bufnr force]
  (let [force (or force false)
        bufnr (or bufnr (n get-current-buf))
        cached (buf->cached bufnr)]
    ; global marks
    (->>
      (vf getmarklist)
      (r.map 
        (fn [data] 
          (let [mark (string.sub data.mark 2 3)] ; mark is 'x
            {: mark 
             :pos data.pos
             :cached (r.get-in buf-cache [bufnr :placed mark])})))
      
      (r.filter 
        (fn [{: mark : cached : pos}]
          (and (= (. pos 1) bufnr)
               (r.upper? mark)
               (or force
                   (not cached)
                   (not= (. pos 2)
                         (. cached :line))))))
      (r.for-each
        (fn [{: mark : pos}]
          (register-mark mark (. pos 2) (. pos 3) bufnr))))

    ; buffer local marks    
    (->>
      (vf getmarklist "%")
      (r.map 
        (fn [data] 
          (let [mark (string.sub data.mark 2 3)] ; mark is 'x
            {: mark 
             :pos data.pos
             :cached (r.get-in buf-cache [bufnr :placed mark])})))
      
      (r.filter 
        (fn [{: mark : cached : pos}]
          (and (r.lower? mark)
               (or force
                   (not cached)
                   (not= (. pos 2)
                         (. cached :line))))))
      (r.for-each
        (fn [{: mark : pos}]
          (register-mark mark (. pos 2) (. pos 3) bufnr))))
    
    ; shown built in marks
    (->>
      shown-builtins
      (r.map 
        (fn [mark]
          {: mark
           :pos (vf getpos (.. "'" mark))
           :cached (r.get-in buf-cache [bufnr :placed mark])}))
      (r.filter
        (fn [{: mark : cached : pos}]
          (and (or (= (. pos 1) 0) ; 0-9 marks return abs bufnr 0
                   (= (. pos 1) bufnr)) 
               (not= (. pos 2) 0)
               (or force
                   (not cached)
                   (not= (. pos 2)
                         (. cached :line))))))
      (r.for-each
        (fn [{: mark : pos}]
          (register-mark mark (. pos 2) (. pos 3) bufnr))))))

(comment
  (refresh-marks nil true))

(defn handle-buf-delete [bufnr]
  (let [bufnr (or bufnr (r.to-number (vf expand "<abuf>")))]
    (r.assoc! buf-cache :bufnr nil)))

(defn main []
  (augroup LibMarks
    {:event :BufEnter
     :pattern :*
     :callback (fn [{: buf}] (refresh-marks buf))}
    {:event :BufDelete
     :pattern :*
     :callback (fn [{: buf}] (handle-buf-delete buf))}))
