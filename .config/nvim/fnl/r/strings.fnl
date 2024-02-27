(module r.strings
  {autoload
   {a aniseed.core
    astr aniseed.string
    r r
    md utils.module
    utils utils}
   require {}
   require-macros [macros]})

(defn split [sep strng]
  "split strings by separator"
  (astr.split strng sep))

(defn upper-first [s]
  "Converts the first character of string to upper case."
  (let [first (-> s (: :sub 1 1) (: :upper))
        rest (s:sub 2)]
    (.. first rest)))

(defn to-lower-case [str]
  "Converts string, as a whole, to lower case."
  (string.lower str))

(defn deburr [str]
  "Deburrs a string"
  (->
    str
    (: :gsub "-" " ")))

(defn lmatch [pattern str]
  "Match a string against a lua-pattern, returning a table of matches"
  (icollect [mtch (string.gmatch str pattern)] mtch))

(defn vmatch [regexp str]
  "Match a string against a vim regex instance, returning a table of matches"
  (let [(beg end) (regexp:match_str str)]
    (if beg
      (values true (str:sub (+ beg 1) end))
      (values false))))

(comment
  (let [r (vim.regex "\\(\\w\\+\\)")]
    (r:match_str "foo"))
  (vmatch (vim.regex "\\(\\w\\+\\)") "     baz  ")
  (vmatch (vim.regex "\\(\\w\\+\\)") ""))


(defn words [str]
  "Split a string into words."
  (let [str (deburr (tostring str))
        words []]
    (each [word (str:gmatch "%S+")]
      (table.insert words word))
    words))

(defn padd-right [char len str]
  "Pads str on the right side if it's shorter than length."
  (let [str-len (length str)
        pad-len (math.max 0 (- len str-len))
        padding (string.rep char pad-len)]
    (.. str padding)))

(comment
  (padd-right "--" 10 "foo"))

(defn- create-compounder [cb]
  "Creates a compounder function for a given callback
  A compounder function is a function that takes a string, split it into words, then
  applies a callback to each word, and finally joins the words back together."
  (fn [str]
    (->>
      str
      (words)
      (a.reduce cb ""))))

; ==<Converters>==

(defn pascal-case [str]
  "converts a string to PascalCase"
  ((create-compounder (fn [acc word] (->> word (string.lower) (upper-first) (.. acc))))
   str))

(defn kebab-case [str]
  "converts a string to kebab-case"
  (vf substitute (vim.trim str) "\\s\\+" "-" "g"))

(comment
  (kebab-case " foo  bar"))

(defn starts-with? [str prefix]
  "Checks if string starts with the given prefix."
  (vim.startswith str prefix))

(defn ends-with? [str suffix]
  "Checks if string ends with the given suffix."
  (vim.endswith str suffix))
