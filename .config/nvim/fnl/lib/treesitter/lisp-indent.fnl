(module lib.treesitter.fennel-indent
  {autoload
   {r r
    parsers nvim-treesitter.parsers
    utils utils}
   require-macros [macros]})

(def- ts vim.treesitter)

(def comment_parsers 
  {:comment true :jsdoc true :phpdoc true})

(fn getline [lnum]
  (or (. (vim.api.nvim_buf_get_lines 0 (- lnum 1) lnum false) 1) ""))

(fn get-first-node-at-line [root lnum col]
  (set-forcibly! col (or col (vim.fn.indent lnum)))
  (root:descendant_for_range (- lnum 1) col (- lnum 1) (+ col 1)))

(fn get-last-node-at-line [root lnum col]
  (let [col (or col (- (length (getline lnum)) 1))]
    (root:descendant_for_range (- lnum 1) col (- lnum 1) (+ col 1))))

(fn node-length [node]
  (let [(_ _ start-byte) (node:start)
        (_ _ end-byte) (node:end_)]
    (- end-byte start-byte)))

(fn find-delimiter [bufnr node delimiter]
  (each [child _ (node:iter_children)]
    (when (= (child:type) delimiter)
      (let [linenr (child:start)
            line (. (vim.api.nvim_buf_get_lines bufnr linenr (+ linenr 1) false) 1)
            end-char [(child:end_)]
            escaped-delimiter (delimiter:gsub "[%-%.%+%[%]%(%)%$%^%%%?%*]" "%%%1")
            (trimmed-after-delim _) (: (line:sub (+ (. end-char 2) 1)) :gsub (.. "[%s" escaped-delimiter "]*") "")]
        (values child (= (length trimmed-after-delim) 0))))))

(fn memoize [___fn___ hash-fn]
  (let [cache (setmetatable {} {:__mode :kv})]
    (fn [...]
      (let [key (hash-fn ...)]
        (when (= (. cache key) nil)
          (local val (___fn___ ...))
          (tset cache key (or (and (not= val nil) val) vim.NIL)))
        (local val (. cache key))
        (or (and (not= val vim.NIL) val) nil)))))

(def- get-indents
  (memoize 
    (fn [bufnr root lang]
      (let [map {:indent.align {}
                 :indent.auto {}
                 :indent.begin {}
                 :indent.branch {}
                 :indent.dedent {}
                 :indent.end {}
                 :indent.ignore {}
                 :indent.zero {}}
            query (ts.query.get lang :indents)]
        (if (not query) 
          map
          (each [id node metadata (query:iter_captures root bufnr)]
            (when (not= (: (. query.captures id) :sub 1 1) "_")
              (tset (. map (. query.captures id)) (node:id)
                    (or metadata {})))
            map)))
     (fn serializer [bufnr root lang] (.. (tostring bufnr) (root:id) "_" lang)))))

(defn get-indent [lnum]
  (print :get-indent lnum)
  (let [bufnr (vim.api.nvim_get_current_buf)
        parser (parsers.get_parser bufnr)]
    (when (or (not parser) (not lnum))
      (let [___antifnl_rtn_1___ (- 1)] (lua "return ___antifnl_rtn_1___")))

    (local root-lang (parsers.get_buf_lang bufnr))

    (var (root lang-tree) nil)

    (parser:for_each_tree (fn [tstree tree]
                            (when (or (not tstree)
                                      (. comment_parsers (tree:lang)))
                              (lua "return "))
                            (local local-root (tstree:root))
                            (when (ts.is_in_node_range local-root (- lnum 1) 0)
                              (when (or (not root)
                                        (>= (node-length root)
                                            (node-length local-root)))
                                (set root local-root)
                                (set lang-tree tree)))))

    (when (not root) 
      (lua "return 0"))

    (local q (get-indents (vim.api.nvim_get_current_buf) root (lang-tree:lang)))
    (local is-empty-line (not= (string.match (getline lnum) "^%s*$") nil))
    (var node nil)

    (if is-empty-line
        (let [prevlnum (vim.fn.prevnonblank lnum)
              indent (vim.fn.indent prevlnum)]
          (var prevline (vim.trim (getline prevlnum)))
          (set node
               (get-last-node-at-line root prevlnum
                                      (- (+ indent (length prevline)) 1)))
          (when (: (node:type) :match :comment)
            (local first-node (get-first-node-at-line root prevlnum indent))
            (local (_ scol _ _) (node:range))
            (when (not= (first-node:id) (node:id))
              (set prevline (vim.trim (prevline:sub 1 (- scol indent))))
              (local col (- (+ indent (length prevline)) 1))
              (set node (get-last-node-at-line root prevlnum col))))
          (when (. (. q :indent.end) (node:id))
            (set node (get-first-node-at-line root lnum))))

        (set node (get-first-node-at-line root lnum)))

    (local indent-size (vim.fn.shiftwidth))
    (var indent 0)
    (local (_ _ root-start) (root:start))

    (when (not= root-start 0)
      (set indent (vim.fn.indent (+ (root:start) 1))))

    (local is-processed-by-row {})
    (when (. (. q :indent.zero) (node:id)) (lua "return 0"))

    (while node
      (when (and (and (and (and (not (. (. q :indent.begin) (node:id)))
                                (not (. (. q :indent.align) (node:id))))
                           (. (. q :indent.auto) (node:id)))
                      (< (node:start) (- lnum 1)))
                 (<= (- lnum 1) (node:end_)))
        (let [___antifnl_rtn_1___ (- 1)] (lua "return ___antifnl_rtn_1___")))

      (when (and (and (and (not (. (. q :indent.begin) (node:id)))
                           (. (. q :indent.ignore) (node:id)))
                      (< (node:start) (- lnum 1)))
                 (<= (- lnum 1) (node:end_)))
        (lua "return 0"))

      (var (srow _ erow) (node:range))
      (var is-processed false)

      (when (and (not (. is-processed-by-row srow))
                 (or (and (. (. q :indent.branch) (node:id))
                          (= srow (- lnum 1)))
                     (and (. (. q :indent.dedent) (node:id))
                          (not= srow (- lnum 1)))))
        (set indent (- indent indent-size))
        (set is-processed true))

      (local should-process (not (. is-processed-by-row srow)))
      (var is-in-err false)

      (when should-process
        (local parent (node:parent))
        (set is-in-err (and parent (parent:has_error))))

      (when (and should-process
                 (and (and (. (. q :indent.begin) (node:id))
                           (or (or (not= srow erow) is-in-err)
                               (. (. (. q :indent.begin) (node:id))
                                  :indent.immediate)))
                      (or (not= srow (- lnum 1))
                          (. (. (. q :indent.begin) (node:id))
                             :indent.start_at_same_line))))
        (set indent (+ indent indent-size))
        (set is-processed true))

      (when (and is-in-err (not (. (. q :indent.align) (node:id))))
        (each [c (node:iter_children)]
          (when (. (. q :indent.align) (c:id))
            (tset (. q :indent.align) (node:id) (. (. q :indent.align) (c:id)))
            (lua :break))))

      (when (and (and (and should-process (. (. q :indent.align) (node:id)))
                      (or (not= srow erow) is-in-err))
                 (not= srow (- lnum 1)))
        (local metadata (. (. q :indent.align) (node:id)))
        (var (o-delim-node o-is-last-in-line) nil)
        (var (c-delim-node c-is-last-in-line) nil)
        (var indent-is-absolute false)
        (if (. metadata :indent.open_delimiter)
            (set (o-delim-node o-is-last-in-line)
                 (find-delimiter bufnr node (. metadata :indent.open_delimiter)))
            (set o-delim-node node))
        (if (. metadata :indent.close_delimiter)
            (set (c-delim-node c-is-last-in-line)
                 (find-delimiter bufnr node
                                 (. metadata :indent.close_delimiter)))
            (set c-delim-node node))
        (when o-delim-node
          (local (o-srow o-scol) (o-delim-node:start))
          (var c-srow nil)
          (when c-delim-node (set (c-srow _) (c-delim-node:start)))
          (if o-is-last-in-line
              (when should-process
                (set indent (+ indent (* indent-size 1)))
                (when c-is-last-in-line
                  (when (and c-srow (< c-srow (- lnum 1)))
                    (set indent (math.max (- indent indent-size) 0)))))
              (if (and (and (and c-is-last-in-line c-srow) (not= o-srow c-srow))
                       (< c-srow (- lnum 1)))
                  (set indent (math.max (- indent indent-size) 0))
                  (do
                    (set indent
                         (+ o-scol (or (. metadata :indent.increment) 1)))
                    (set indent-is-absolute true))))
          (var avoid-last-matching-next false)
          (when (and (and c-srow (not= c-srow o-srow)) (= c-srow (- lnum 1)))
            (set avoid-last-matching-next
                 (or (. metadata :indent.avoid_last_matching_next) false)))
          (when avoid-last-matching-next
            (if (<= indent (+ (vim.fn.indent (+ o-srow 1)) indent-size))
                (set indent (+ indent (* indent-size 1))) (set indent indent)))
          (set is-processed true)
          (when indent-is-absolute (lua "return indent"))))
      (tset is-processed-by-row srow
            (or (. is-processed-by-row srow) is-processed))
      (set node (node:parent)))
    indent))

(var indent-funcs {})

(defn attach [bufnr] 
  (tset indent-funcs bufnr vim.bo.indentexpr)
  (set vim.bo.indentexpr (utils.viml-fn-bridge *module-name* "get-indent")))

(defn detach [bufnr] 
  (set vim.bo.indentexpr (. indent-funcs bufnr)))

(defn main []
  (augroup LibLispIndent
    {:event [:FileType]
     :pattern :fennel
     :callback
     (r.void
       (fn set-indent []
         (command! :LispIndentOn #(attach (n get-current-buf)))
         (command! :LispIndentOff #(detach (n get-current-buf)))
         (when false ; needs more testing
           (let [bufnr (n get-current-buf)]
             (attach bufnr)
             (n buf-attach 0 false
               {:on_detach #(detach bufnr)
                :on_reload #nil})))))}))
