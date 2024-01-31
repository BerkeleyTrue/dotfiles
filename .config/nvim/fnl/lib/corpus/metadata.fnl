(module lib.corpus.metadata

  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require-macros [macros]})

(def metadata-key-value-pattern "\\v^\\s*(\\w+)\\s*:\\s*(\\S.{-})\\s*$")

; TODO: use proper yaml parser, or switch to toml metadata
(defn get-raw []
  "get yaml metadata in markdown file
  returns a list of strings"
  (if (r.re-includes? (vim.fn.getline 1) "\\v^---\\s*$")
    (let [last-ln (vim.fn.line "$")]
      (var done? false)
      (var cur-line 2)
      (accumulate [out []
                   _ ln (ipairs (vim.fn.range 2 last-ln))
                   &until done?]
        (do
          (case (vim.fn.getline ln)
            (where l (r.re-includes? l "\\v^\\s*$")) (r.conj out l)
            (where l (r.re-includes? l "\\v^---\\s*$")) (do (set done? true) out)
            l (let [kv (vim.fn.matchlist l metadata-key-value-pattern)]
                (if (= (length kv) 0)
                  ; no kv match, bad metadata, exit
                  (do (set done? true) [])
                  (r.conj out (. kv 1))))))))
    []))


(defn get []
  "parse raw metadata into a dictionary"
  (r.reduce
    (fn [acc l]
      (let [kv (vim.fn.matchlist l metadata-key-value-pattern)]
        (if (= (length kv) 0)
          acc
          (r.assoc acc (. kv 2) (. kv 3)))))
    {}
    (get-raw)))

(defn delete []
  (let [raw (get-raw)]
    (when (not (r.empty? raw))
      ; delete metadata from buffer, plus the two separators
      (vim.fn.deletebufline "" 1 (+ (r.size raw) 2)))))

(defn update [data]
  (delete)
  (let [lines ["---"]
        keys (r.keys data)
        lines (->>
                data
                (r.to-pairs)
                (r.map (fn [[ky val]] (.. ky ": " val)))
                (r.concat lines))
        lines (r.conj lines "---")]
    (vim.fn.append 0 lines)
    ; num of keys, plus 2 separators, plus 1 empty line
    (let [nxt (+ (r.size data) 2 1)]
      (when (not (r.re-includes? (vim.fn.getline nxt) "\\v^\\s*$"))
        (vim.fn.append (- nxt 1) "")))))

(defn get-title [file]
  "generate a title from filename"
  (vim.fn.fnamemodify file ":t:r"))

(defn update-file [file]
  (let [file (or file (vim.fn.expand "%"))
        title (get-title file)
        metadata (get)]
    (update (r.assoc metadata "title" title))))
