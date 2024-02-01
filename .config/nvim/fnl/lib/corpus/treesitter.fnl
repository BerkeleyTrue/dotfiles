(module lib.corpus.treesitter
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts nvim-treesitter.compat
    parsers nvim-treesitter.parsers}
   require {query nvim-treesitter.query}
   require-macros [macros]})

(def toml-query "(plus_metadata) @frontmatter")
(def yaml-query "(minus_metadata) @frontmatter")

(defn parse-query [query]
  (let [(ok? parsed) (pcall (fn [] (vim.treesitter.query.parse "markdown" query)))]
    (if ok?
      parsed
      (values nil parsed))))

(defn parse-yaml []
  (parse-query yaml-query))

(defn parse-toml []
  (parse-query toml-query))

(defn extract-frontmatter []
  "grabs the frontmatter from the current buffer and returns it as a string.
  If there is an error parsing the frontmatter, it prints the error to the console and returns nil.
  If the frontmatter is successfully parsed, it returns the frontmatter as a string."
  (let [bufnr (n get_current_buf)
        tree (parsers.get_parser bufnr)
        root (: (. (tree:trees) 1) :root)
        (start-row _ end-row _) (root:range)
        get-first-match (fn [parsed]
                          ((query.iter_prepared_matches parsed root bufnr start-row end-row)))
        get-text (fn [mtch] (-> (. mtch :frontmatter :node) (vim.treesitter.get_node_text bufnr)))]
    (case-try (parse-toml)
      parsed (get-first-match parsed)
      mtch (get-text mtch)
      text {:text text :toml true}
      (catch
        _ (case-try (parse-yaml)
            parsed (get-first-match parsed)
            mtch (get-text mtch)
            text {:text text :yaml true}
            (catch
              _ (do (a.println "Nothing found") {})
              (nil err) (do
                          (a.print "Error parsing frontmatter:" err)
                          {})))
        (nil err) (do
                    (a.print "Error parsing frontmatter:" err)
                    {})))))

(command! :CorpusExtractFrontmatter (fn [] (a.println (extract-frontmatter))))
