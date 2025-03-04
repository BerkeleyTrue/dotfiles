(module lib.corpus.metadata
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts lib.corpus.treesitter}
   require
   {toml lib.toml
    yaml lyaml}
   require-macros [macros]})

(defn date []
  (let [{: year : month : day} (os.date "*t")]
    (.. year "-" (if (< month 10) "0" "") month "-" (if (< day 10) "0" "") day)))

(comment (date))

(defn decode-toml [str]
  (toml.parse str))

(defn encode-toml [data]
  (values true (toml.encode data)))

(comment
  (encode-toml {:title "hello" :tags [] :updated-at (date)})) ; empty tables get nested as dict instead of array

(defn get-frontmatter []
  "get metadata from buffer as a lua table"
  (let [raw (ts.extract-frontmatter)]
    (case raw
      (where {:yaml yamls} (not (r.empty? yamls)))
      (pcall yaml.load yamls)

      (where {:toml toml} (not (r.empty? toml)))
      (case (decode-toml toml)
        (false err) (values false (.. "Error decoding toml: " (a.pr-str err)))
        (true data) (values true data))

      _ {})))

(comment
  (command! :CorpusGetFrontmatter
            (fn [] (a.println :frontmatter (get-frontmatter)))))

(defn update-frontmatter [data]
  (case (encode-toml data)
    (false err) (a.println "error updating frontmatter" err)
    (true str) (ts.replace-frontmatter (.. "+++\n" str "\n+++\n"))))

(defn get-title [file]
  "generate a title from filename"
  (vim.fn.fnamemodify file ":t:r"))

(defn update-file [opts]
  (let [{: force?
         : temp?
         : tags} (or opts {})
        file (vim.fn.expand "%")
        title (if temp? "" (get-title file))
        day (date)
        (ok? metadata?) (get-frontmatter)
        metadata (if (and ok? (r.exists? metadata?)) metadata? {})
        old-tags (or metadata.tags [])
        tags (->> 
               (r.concat old-tags tags)
               (r.uniq))
        created-at (or metadata.created-at day)]
    (when (or force? ok?)
      (update-frontmatter
        (r.merge
          metadata
          {:title (r.kebab-case title)
           :updated-at day
           :created-at created-at
           :tags  tags})))))
