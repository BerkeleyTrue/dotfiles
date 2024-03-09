(module r.strings
  {autoload
   {a aniseed.core
    astr aniseed.string
    r r
    lang r.lang}
   require {}
   require-macros [macros]})

(defn to-string [x]
  "Converts a value to a string"
  (->
    x
    (or "")
    (tostring)))

(comment
  (to-string)
  (to-string nil)
  (to-string {})
  (to-string "hello world"))

(defn split [sep str]
  "split strings by separator, which can be a lua pattern"
  (->
    str
    (to-string)
    (astr.split sep)))

(comment
  (split "[%s,]+" "foo,bar baz"))

(defn upper-first [str]
  "Converts the first character of string to upper case."
  (let [str (to-string str)
        first (->
                str
                (string.sub 1 1)
                (string.upper))
        rest (string.sub str 2)]
    (.. first rest)))

(comment
  (upper-first "foo") ; "Foo"
  (upper-first "FOO")) ; "FOO"

(defn to-lower-case [str]
  "Converts string, as a whole, to lower case."
  (->
    str
    (to-string)
    (string.lower)))

(defn capitalize [word]
  "Capitalize the first character of a string and lower case the rest."
  (->>
    word
    (to-string)
    (string.lower)
    (upper-first)))

(comment
  (capitalize "hello world") ; "Hello world"
  (capitalize "HELLO WORLD")) ; "Hello world"

(defn trim [str]
  "Removes leading and trailing whitespace from string."
  (->
    str
    (to-string)
    (vim.trim)))

(comment (trim "  foo  "))

(defn lmatch [pattern str]
  "Match a string against a lua-pattern using gmatch, returning a table of matches"
  (icollect [mtch (string.gmatch (to-string str) pattern)] mtch))

(comment
  (lmatch "%S+" "foo bar baz"))

(defn vmatch [regexp str]
  "Match a string against a vim regex instance, returning a table of matches"
  (let [str (to-string str)
        (beg end) (regexp:match_str str)]
    (if beg
      (values true (string.sub str (+ beg 1) end))
      (values false))))

(comment
  (let [r (vim.regex "\\(\\w\\+\\)")]
    (r:match_str "foo"))
  (vmatch (vim.regex "\\(\\w\\+\\)") "     baz  ")
  (vmatch (vim.regex "\\(\\w\\+\\)") ""))

(defn lsub [pattern repl str]
  "Replace all occurrences of a pattern in a string with a replacement"
  (pick-values 1
    (->
      str
      (to-string)
      (string.gsub pattern repl))))

(comment
  (lsub "%u" " %1" "fooBarBaz"))

(defn words [str]
  "Split a string into words."
  (->>
    str
    (lsub "%u" " %1")
    (trim)
    (lmatch "%S+")))

(comment
  (words "foo Bar baz")
  (words "fooBarBaz"))

(defn capitalize-words [str]
  (->>
    str
    (words)
    (r.map capitalize)
    (r.join " ")))

(comment
  (capitalize-words "hello world") ; "Hello World"
  (capitalize-words "HELLO WORLD")) ; "Hello World"

(defn padd-right [char len str]
  "Pads str on the right side if it's shorter than length."
  (let [str (to-string str)
        str-len (length str)
        pad-len (math.max 0 (- len str-len))
        padding (string.rep char pad-len)]
    (.. str padding)))

(comment
  (padd-right "--" 10 "foo"))

; ==<Converters>==

(defn pascal-case [str]
  "converts a string to PascalCase"
  (->>
    str
    (words)
    (r.map capitalize)
    (r.join "")))

(comment
  (string.gsub "fooBarBaz" "%u" " %1")
  (pascal-case " foo  bar")
  (pascal-case "fooBarBaz"))

(defn kebab-case [str]
  "converts a string to kebab-case"
  (->>
    str
    (words)
    (r.map to-lower-case)
    (r.join "-")))

(comment
  (kebab-case " foo  bar") ; "foo-bar"
  (kebab-case "fooBar")) ; "foo-bar"

(defn starts-with? [str prefix]
  "Checks if string starts with the given prefix."
  (vim.startswith str prefix))

(comment
  (starts-with? "foo" "f"))

(defn ends-with? [str suffix]
  "Checks if string ends with the given suffix."
  (vim.endswith str suffix))

(defn lower? [char]
  "Checks if a character is lower case"
  (<= 97 (string.byte char) 122))

(defn upper? [char]
  "Checks if a character is upper case"
  (<= 65 (string.byte char) 90))

(defn letter? [char]
  "Checks if a character is a letter"
  (or (lower? char) (upper? char)))
