(module lib.buffers
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(defn list-visible []
  "Return a list of the visible buffer numbers of buffers."
  (->>
    (n list-wins)
    (r.map #{:buf (n win-get-buf $)
             :win $})
    (r.filter #(= (bo buftype $.buf) ""))
    (r.map #(r.get $ :buf))))

(defn get-help-buffer []
  "Return the buffer number of the help buffer"
  (->>
    (n list-wins)
    (r.map #{:buf (n win-get-buf $)
             :win $}) 
    (r.filter #(= (bo buftype $.buf) :help))
    (r.map #(r.get $ :buf))
    (r.head)))

(defn get-help-window []
  "Return the window number of the help window"
  (->>
    (n list-wins)
    (r.map #{:buf (n win-get-buf $)
             :win $}) 
    (r.filter #(= (bo buftype $.buf) :help))
    (r.head)
    (#(r.get $ :win))))
