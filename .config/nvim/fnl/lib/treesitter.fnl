(module lib.treesitter
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts nvim-treesitter.ts_utils
    tsnode lib.treesitter.tsnode}
   require {}
   require-macros [macros]})

(tset *module* :tsnode tsnode)

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
  "Update the current selection around node for buffer.
  Defaults to current buffer."
  (let [bufnr (or bufnr (n get-current-buf))]
    (ts.update_selection bufnr node)))
