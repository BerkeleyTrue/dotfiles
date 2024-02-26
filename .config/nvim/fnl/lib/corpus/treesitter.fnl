(module lib.corpus.treesitter
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    parsers nvim-treesitter.parsers
    ts lib.treesitter
    tsnode lib.treesitter.tsnode}
   require
   {query nvim-treesitter.query}
   require-macros [macros]})

(defn parse-query [query lang]
  (let [(ok? parsed) (pcall (fn [] (vim.treesitter.query.parse (or lang "markdown") query)))]
    (if ok?
      parsed
      (values nil parsed))))

(defn get-root [bufnr]
  (let [bufnr (or bufnr (n get_current_buf))
        parser (parsers.get_parser bufnr)]
    (: (. (parser:trees) 1) :root)))

(defn get-matches [parsed-query bufnr]
  "get matches for parsed query as an iterator. If not matches found, iterator returns nil."
  (let [root (get-root bufnr)
        (start-row _ end-row _) (root:range)]
    (query.iter_prepared_matches parsed-query root bufnr start-row end-row)))

(defn replace-node [bufnr node new-text]
  (let [lsp-range (tsnode.range node)]
     (vim.lsp.util.apply_text_edits
       [{:range lsp-range :newText new-text}]
       bufnr
       :utf-8)))

; ## Extractions
(def front-matter-query "(plus_metadata) @toml (minus_metadata) @yaml")

(defn- format-frontmatter [s]
  (->> s
       (r.split "\n") ; split the string into lines
       (r.map vim.trim) ; trim each line
       (r.filter (r.negate r.empty?)) ; remove empty lines
       (r.join "\n"))) ; join the lines back together

(defn extract-frontmatter []
  "Grabs the frontmatter from the current buffer and returns it as a string.
  If there is an error parsing the frontmatter, it prints the error to the console and returns nil.
  If the frontmatter is successfully parsed, it returns the frontmatter as a string."
  (let [bufnr (n get_current_buf)
        get-text (fn [mtch]
                   {:yaml
                    (or
                      (-?>
                        (?. mtch :yaml :node)
                        (vim.treesitter.get_node_text bufnr)
                        (vim.fn.substitute "\\v\\-\\-\\-\\n?" "" "g")
                        (format-frontmatter))
                      "")
                    :toml
                    (or
                      (-?>
                        (?. mtch :toml :node)
                        (vim.treesitter.get_node_text bufnr)
                        (vim.fn.substitute "\\v\\+\\+\\+\\s*\\n?" "" "g")
                        (format-frontmatter))
                      "")})]
    (case-try (parse-query front-matter-query)
      parsed ((get-matches parsed bufnr))
      mtchs (if (r.empty? mtchs) {} (get-text mtchs))
      (catch
        (nil err) (do
                    (a.println "Error parsing frontmatter:" err)
                    {})
        nil {} ; no matches
        _ (do
            (a.println "Unknown error parsing frontmatter.")
            {})))))

(comment
  (command! :CorpusExtractFrontmatter (fn [] (a.println (extract-frontmatter)))))

(defn replace-frontmatter [new-text]
  "Replaces the frontmatter in the current buffer with the given new-text."
  (let [bufnr (n get_current_buf)
        parsed (parse-query front-matter-query)
        matches ((get-matches parsed bufnr))]
    (if (r.empty? matches)
      (vf append "0" (r.split "\n" new-text))
      (let [node (or (?. matches :yaml :node) (?. matches :toml :node))]
        (replace-node bufnr node new-text)))))

(comment
  (command! :CorpusReplaceFrontmatter
            (fn [{: args}] (a.println :foo (replace-frontmatter args)))))

(def link-ref-def-query "(link_reference_definition
                          (link_label) @link_label
                          (link_destination) @link_destination)")

(comment
  (vf substitute "[label]" "\\v\\[|\\]" "" "g") ; => "label"
  (vf substitute "<./destination>" "\\v\\<|\\>" "" "g")) ; => "./destination"

(defn extract-link-reference-definitions []
  "Extract link reference definitions from the current buffer and return them as a list of maps.
  Each map has a :label and a :destination key. If there is an error parsing the link reference definitions,
  an empty list is returned"
  (let [bufnr (n get_current_buf)
        get-text (fn get-text [mtch]
                   {:label
                    (or (-?>
                          (. mtch :link_label :node)
                          (vim.treesitter.get_node_text bufnr)
                          (vim.fn.substitute "\\v\\[|\\]" "" "g"))
                        "")
                    :destination
                    (or (-?>
                          (. mtch :link_destination :node)
                          (vim.treesitter.get_node_text bufnr)
                          (vim.fn.substitute "\\v\\<|\\>" "" "g"))
                        "")})]

    (case-try (parse-query link-ref-def-query)
      parsed (icollect [mtc (get-matches parsed bufnr)] mtc)
      matches (->> matches (r.map get-text))
      (nil err) (do
                  (a.println "Error parsing link reference definitions:" err)
                  []))))

(comment (command! :CorpusExtractLinkRefDef (fn [] (a.println (extract-link-reference-definitions)))))

(def link-shortcut-query "(shortcut_link (link_text) @shortcut_link)")

(defn extract-link-shortcuts []
  "Grabs all the link shortcut labels from the current buffer and returns them as a list of strings."
  (let [bufnr (n get_current_buf)
        parser (parsers.get_parser bufnr)
        inlines (. (parser:children) :markdown_inline)
        get-text (fn get-gt [mtch] (-> (. mtch :shortcut_link :node) (vim.treesitter.get_node_text bufnr)))]

    (var out [])
    (when (r.not-empty? inlines)
      (case (parse-query link-shortcut-query :markdown_inline)
        (nil err) (do (a.println "Error parsing link shortcuts:" err) [])
        parsed-query
        (do
          (inlines:for_each_tree
            (fn [tree]
              (let [root (tree:root)]
                (icollect [_ node (parsed-query:iter_captures root 0)]
                  (table.insert out (vim.treesitter.get_node_text node bufnr))))))
          out)))))

(comment
  (command! :CorpusExtractLinkShortcuts
            (fn [] (a.println (extract-link-shortcuts)))))

(defn get-node-under-cursor []
  "Get the node under the cursor. Nil, if nothing is found"
  (let [[bufnr lnum col _] (vf getpos ".")
        lnum (- lnum 1)
        lang-parsers (parsers.get_parser bufnr)
        lang-tree (lang-parsers:language_for_range [lnum col lnum col])]
    (when-let [node (accumulate [node nil
                                 _ tree (ipairs (lang-tree:trees))
                                 &until (not (r.nil? node))]
                      (when-let [root (tree:root)]
                         (if (vim.treesitter.is_in_node_range root lnum col)
                           (root:named_descendant_for_range lnum col lnum col))))]
      {:text (vim.treesitter.get_node_text node bufnr)
       :node node
       :type (node:type)})))


(comment
  (command! :CorpusGetNodeUnderCursor (fn [] (a.println (get-node-under-cursor)))))
