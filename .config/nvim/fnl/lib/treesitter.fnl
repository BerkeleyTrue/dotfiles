(module lib.treesitter
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts nvim-treesitter.ts_utils}
   require {}
   require-macros [macros]})

(defn get-node-under-cursor []
  "Get the node under the cursor."
  (ts.get_node_at_cursor))

(defn get-node-text [node bufnr metatable]
  "Get the text of a node.
  Optionally include the desired buffer and "
  (let [node (or node (get-node-under-cursor))
        bufnr (or bufnr (n get-current-buf))]
    (vim.treesitter.get_node_text node bufnr metatable)))

(comment (get-node-text))

(defn update-selection [node bufnr]
  "Update the current selection around node for burrer.
  Defaults to current buffer."
  (let [bufnr (or bufnr (n get-current-buf))]
    (ts.update_selection bufnr node)))


;; node methods

(defn range [node]
  "Returns the node's range as a table of
  {:start {:line row
           :character col
           :col col
           :row row}
   :end {:line row
         :character col
         :col col
         :row row}}."
  (let [(start-row start-col end-row end-col) (vim.treesitter.get_node_range node)]
    {:start {:row start-row
             :line start-row
             :character start-col
             :col start-col}
     :end {:row end-row
           :line end-row
           :character end-col
           :col end-col}}))

(defn first [node]
  "Get the first child of a node."
  (node:named_child 0))

(defn last [node]
  "Get the last child of a node"
  (node:child (node:named_child_count)))

(defn start [node]
  "Get the start of a node."
  (let [(row col bytes) (node:start)]
    {: row
     : col
     : bytes}))

(defn end [node]
  "Get the end of a node."
  (let [(row col bytes) (node:end_)]
    {: row
     : col
     : bytes}))
