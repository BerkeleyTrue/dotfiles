(module lib.log
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(def- modes
  [{:name :trace :hl :Comment}
   {:name :debug :hl :Comment}
   {:name :info  :hl :None}
   {:name :warn  :hl :WarningMsg}
   {:name :error :hl :ErrorMsg}])

(def- default-config
  {:level :info
   :use_console true
   :use_file true})

(defn create [config]
  (assert (r.not-empty? config.name) "config.name is required")
  (let [config (r.merge default-config config)
        outfile (string.format
                  "%s/%s.log"
                  (vf stdpath :data)
                  config.name)
        obj {}
        level->idx  (->>
                      modes
                      (r.to-pairs)
                      (r.map (fn [i val] [val.name i]))
                      (r.from-pairs))]

    (fn log-at-level [{: level : name : hl} msg]
      (when (> level (. level->idx config.level))
        (let [nameupper (string.upper name)
              info (debug.getinfo 2 :Sl)
              lineinfo (.. info.short_src ":" info.currentline)]

          (when config.use_console
            (command echohl hl)

            (->
              (string.format
                "[%-6s%s] %s: %s"
                nameupper (os.date "%H:%M:%S") lineinfo msg)
              (r.split "\n")
              (r.for-each
                (fn [line]
                  (command echom
                    (string.format
                      "\"[%s] %s\""
                      config.name (vim.fn.escape line "\""))))))

            (command echohl "NONE"))

          (when config.use_file
            (let [str (string.format
                        "[%-6s%s] %s: %s\n"
                        nameupper (os.date) lineinfo msg)]
              (doto (io.open outfile :a)
                (: :write str)
                (: :close)))))))

    (->>
      modes
      (r.to-pairs)
      (r.reduce
        (fn [acc [idx {: hl : name}]]
          (tset acc name
            (fn [...]
              (log-at-level
                {:level idx
                 : name
                 : hl}
                (a.str ...))))

          (tset acc (.. name "-fmt")
            (fn [fmt & args]
              (let [inspected (->>
                                args
                                (r.map vim.inspect))
                    msg (string.format fmt (unpack inspected))]
                (log-at-level
                  {:level idx
                   : name
                   : hl}
                  msg)))))
        {}))))
