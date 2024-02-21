(module lib.treesitter.queries
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(comment
  (def- pattern "%.?([%w-_?!*=/]+)$")
  (string.match "run-main" pattern)
  (string.match "function" pattern)
  (string.match "md.prequire" pattern)
  (length "md.prequire")
  (length "prequire")
  (string.match "="  pattern)
  (string.match "/"  pattern)
  (string.match "foo.method!" pattern))

(defn- has-offset [s pattern]
  (let [m (string.match s pattern)]
    (and (r.exists? m)
         (not= (length s) (length m)))))

(defn- get-offset [s pattern]
  (let [m (string.match s pattern)]
     (- (length s) (length m))))

(comment
  (has-offset "run-main" pattern)
  (get-offset "md.prequire" pattern))

(defn add-match-directive []
  (vim.treesitter.query.add_directive
    "match-offset!"
    ; pred = (match! <capture-id> <pattern> <inv>)
    (fn [mtch _ bufnr pred metadata]
      (let [from-end (. pred 4)
            capture-id (. pred 2)
            pattern (. pred 3)
            node (. mtch capture-id)]
        (when (r.exists? node)
          (when (not (. metadata capture-id))
            (tset metadata capture-id {}))
          (let [text (vim.treesitter.get_node_text node bufnr {:metadata (. metadata capture-id)})
                offset? (has-offset text pattern)
                offset (if offset? (get-offset text pattern) 0)
                range (or (. (. metadata capture-id) :range) [(node:range)])]

            (when (> offset 0)
              (if from-end
                (tset range 4 (- (. range 4) offset))
                (tset range 2 (+ (. range 2) offset)))

              (when (< (. range 2) (. range 4))
                (tset (. metadata capture-id) :range range)))))))))

(defn main []
  (add-match-directive))
