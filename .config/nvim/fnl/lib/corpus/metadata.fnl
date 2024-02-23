(module lib.corpus.metadata
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts lib.corpus.treesitter}
   require
   {toml toml
    yaml lyaml}
   require-macros [macros]})

(defn date []
  (let [{: year : month : day} (os.date "*t")]
    (toml.Date.new year month day)))

(defn decode-toml [str]
  (pcall toml.decode str))

(defn encode-toml [data]
  (pcall toml.encode data))

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

(defn update-file [file]
  (let [file (or file (vim.fn.expand "%"))
        title (get-title file)
        day (date)
        (ok? metadata) (get-frontmatter)
        created-at (or (. metadata :created-at) day)]
    (when ok?
      (update-frontmatter (r.merge
                            metadata
                            {:title title
                             :created-at created-at
                             :updated-at day})))))
