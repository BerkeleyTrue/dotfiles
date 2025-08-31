(module lib.dotenv
  {autoload
   {a aniseed.core
    r r}
   require {}
   import-macros []
   require-macros [macros]})

(def- uv vim.loop)

(defn get-env-path []
  (let [files (vim.fs.find ".env" {:upward true :type :file})]
    (if (not (r.empty? files))
      (a.first files)
      nil)))

(comment
  (get-env-path))

(defn read-file [path]
  (let [fd (match (uv.fs_open path :r 438)
              (nil msg) nil
              fd fd)
        stat (match (uv.fs_fstat fd)
               (nil msg) nil
               stat stat)
        data (match (uv.fs_read fd stat.size 0)
               (nil msg) nil
               data data)]
    (match (uv.fs_close fd)
      (nil msg) nil
      _ data)))

(comment
  (read-file (get-env-path)))

(defn parse-data [data]
  (let [vals (r.split "\n" data)]
    (->> vals
         (r.map r.trim)
         (r.reject #(r.empty? $))
         (r.reject #(r.starts-with? $ "#"))
         (r.map #(->> $ 
                      (r.split "=")
                      (r.map r.trim)
                      (r.reject r.empty?)
                      ((fn [[key & val]] [key (r.join "=" val)]))))
         (r.reject #(= (r.size $) 1))
         (r.map (fn [[key val]] [key (r.lsub "'" "" (r.lsub "\"" "" val))]))
         (r.from-pairs))))

(defn load []
  (-> (get-env-path)
      (read-file)
      (parse-data)))

(comment
  (load))
