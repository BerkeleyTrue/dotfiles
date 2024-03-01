(module lib.log
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(def- settings
  {:namespaces nil
   :names []
   :skips []})

(defn save [namespace])

(defn enable [namespaces]
  (save namespaces)
  (a.merge! 
    settings
    {:namespaces namespaces
     :names []
     :skips []})
  (->>
    (r.split "[%s,]+")
    (r.for-each 
      (fn [ns] 
        (if (r.starts-with? ns "-")
          (r.update settings :skips #(r.conj $ (string.sub ns 2)))
          (r.update settings :names #(r.conj $ ns)))))))

(defn disable []
  "Disables all namespaces, returning those old namespaces."
  (let [namespaces (->>
                     settings.names
                     (r.concat (r.map #(.. "-" $) settings.skips))
                     (r.join ","))]
    namespaces))
    

(defn enabled [name]
  (if
    (r.ends-with? name "*") true
    (r.includes? settings.skips name) false
    (r.includes? settings.names name) true
    false))


(defn create [namespace]
  (var prev-ts nil)
  (var nscache nil)
  (var enabledCache nil)

  (fn _enabled []
    ; used to cache the enabled state of the namespace
    ; and invalidate if settings.namespaces
    (when (not= nscache settings.namespaces)
      (set nscache settings.namespaces)
      (set enabledCache (enabled namespace)))

    enabledCache)

  (fn log [& args]
    (when (_enabled namespace)
      (let [curr (os.time)
            delta (- curr (or prev-ts curr))
            output (r.apply a.pr-str args)
            output (string.format
                     "%s: %s +%s"
                     namespace
                     output
                     delta)]
                     
        (set prev-ts curr)
        (n echo [[output]] true {})))))
