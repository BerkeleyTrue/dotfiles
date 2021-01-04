(module ts.indents
  {require {: r
            nutils utils
            parsers nvim-treesitter.parsers
            queries nvim-treesitter.query
            utils nvim-treesitter.ts_utils}
   require-macros [macros]})

(defn get-indent-width []
  (or (and (< nutils.bo.softtabstop 0) nutils.bo.shiftwidth) nutils.bo.tabstop))

; search through child recur
; if node has children
; search nodes children
; can be optimized
(defn search-siblings [iter lnum]
  (let [node (iter)]
    (if (not node) nil
      (let [(start-row _ end-row) (node:range)]
        ; (nutils.print "snode: " (tostring node))
        ; (nutils.print :range start-row lnum end-row)
        ; (nutils.print :child_count (node:child_count))
        ; (nutils.print :child-in-range (and (> (node:child_count) 0) (< start-row lnum) (<= lnum end-row)))
        (if (= start-row lnum) node
          ; if node has children
          ; and start row is above line
          ; and end row is above line
          ; then recur
          (let [node (if (and (> (node:child_count) 0) (< start-row lnum) (<= lnum end-row)) (*module*.aniseed/locals.search-nodes-children node lnum) nil)]
            (if node node
              (search-siblings iter lnum))))))))

; grab node childs
(defn- search-nodes-children [node lnum]
  (if (not node) nil
    (search-siblings (node:iter_children) lnum)))

(defn- get-node-at-line [root lnum]
  (let [node (search-nodes-children root lnum)]
    (if node node
      (let [wrapper (root:descendant_for_range lnum 0 lnum -1)
            child (wrapper:child 0)]
        (or child wrapper)))))

(defn- get-indents-raw [bufnr]
  (let [indents (->>
                  (queries.get_capture_matches bufnr "@indent.node" "indents")
                  (r.default-to [])
                  (r.map #[(tostring $1) true])
                  (r.from-pairs))

        branches (->>
                   (queries.get_capture_matches bufnr "@branch.node" "indents")
                   (r.default-to [])
                   (r.map #[(tostring $1) true])
                   (r.from-pairs))
        ignore (->>
                 (queries.get_capture_matches bufnr "@ignore.node" "indents")
                 (r.default-to [])
                 (r.map #[(tostring $1) true])
                 (r.from-pairs))]

    {: indents : branches : ignore}))

(def- get-indents
  (utils.memoize_by_buf_tick get-indents-raw))

(defn- get-root [parser]
  (->
    (parser:parse)
    (r.head)
    (: :root)))

(defn- get-non-branch-node [branches node]
  (let [name (tostring node)
        branch? (. branches name)]
    (if (not branch?) node
      (let [parent (node:parent)]
        (if (not parent) nil
          (get-non-branch-node branches parent))))))

(defn- get-indent-size [{: indents : node : indent-width : cursize : start-line : lnum}]
  ; (nutils.print cursize)
  (if (not node) (or cursize 0)
    (let [cursize (or cursize 0)
          name (tostring node)
          indent? (. indents name)
          last-line (or start-line -1)
          (this-line _ end-line) (node:range)
          next-size (if
                      (and
                        ; node is marked as indentable
                        indent?
                        ; node is not on the same line as last line (avoid double up)
                        (not= this-line last-line)
                        ; node is not outside of lnum (parent must encircle child)
                        ; this prevents closing brackets from getting the wrong indenting
                        (>= end-line lnum))
                      (+ cursize indent-width)
                      cursize)]
          ; for debugging
          ;_ (nutils.print {: last-line : this-line : name : next-size : indent?})]
      (get-indent-size {: indents :node (node:parent) : indent-width :cursize next-size :start-line this-line : lnum}))))

(defn get-indent []
  (let [lnum (. nutils.v :lnum)
        parser (parsers.get_parser)
        curbuf (nutils.get_current_buf)
        {: indents : branches : ignore} (get-indents curbuf)]
    (if (not (and parser lnum (not (r.empty? indents)))) -1
      (let [node (->>
                   (get-node-at-line (get-root parser) (- lnum 1))
                   (get-non-branch-node branches))
            indent-width (get-indent-width)]
        (if (. ignore (tostring node))
          -1
          (get-indent-size {: indents : node : indent-width :cursize 0 : lnum}))))))

(def- indent-funcs {})

(defn attach [bufnr]
  (tset indent-funcs bufnr (. nutils.bo :indentexpr))
  (tset nutils.bo :indentexpr (nutils.viml->luaexp *module-name* (sym->name get-indent))))

(defn detach [bufnr]
  (tset vim.bo :indentexpr (. indent-funcs bufnr)))
