(module lib.sexps.forms
  {autoload
   {a aniseed.core
    r r
    md utils.module
    utils utils
    ts lib.treesitter
    tsnode lib.treesitter.tsnode}
   require {}
   require-macros [macros]})


(def- forms {:list true
             :array true
             :table true})

(def- offset {:list 1
              :array 1
              :table 1})

(defn form? [node]
  "Is a node a form"
  (= (. forms (node:type)) true))

(defn ->offset [form]
  "Get the offset of a form."
  (. offset (form:type)))

(defn- find-current-form [node]
  "Find the current form at the cursor position.
  If the cursor is not on a form, it will find the nearest form up the tree.
  Returns nil if no form is found."
  (if (form? node)
    node
    (when-let [parent (node:parent)]
      (find-current-form parent))))

(defn find []
  "Get the current form under the cursor."
  (find-current-form (ts.get-node-under-cursor)))

(defn- find-root-form [node]
  "Find the root form of the current form.
  Returns nil if no root form is found"
  (let [{: col} (tsnode.start node)]
    (if (and (form? node)
             (= col 0))
      node
      (when-let [parent (node:parent)]
        (find-root-form parent)))))

(defn find-root []
  "Get the root form under the cursor."
  (find-root-form (ts.get-node-under-cursor)))


(defn elem? [node]
  (not= (node:type) :comment))

(defn next-elem [node]
  "Get the next element in a form.
  Returns nil if no next element is found."
  (when-let [node (node:next_sibling)]
    (if (elem? node)
      node
      (next-elem node))))

(defn prev-elem [node]
  "Get the prev element in a form.
  Returns nil if no next element is found."
  (when-let [node (node:prev_sibling)]
    (if (elem? node)
      node
      (next-elem node))))
