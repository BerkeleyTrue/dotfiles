(module slackline.highlight
  {:require {: utils
             : r
             :t theme}})

(defn- create-hl-comp [str]
  (.. "%#" str "#"))

(defn- format-hl-name [name]
  (->>
    name
    (r.deburr)
    (.. "Slackline ")
    (r.pascal-case)))

(defn hl-comp [name]
  (->
    name
    (format-hl-name)
    (create-hl-comp)))

(defn add-group [name fg bg]
  (t.add-group (format-hl-name name) fg bg))
