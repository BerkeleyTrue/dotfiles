(module utils
  {require
   {a aniseed.core
    nvim aniseed.nvim
    nutils aniseed.nvim.util
    r r
    str aniseed.string}})

(def vim (. _G :vim))

;; hack to pass through nvim
(setmetatable *module* {:__index nvim})

(defn- viml-name-format [s] (-> s (: :gsub :%. "_") (: :gsub :%- "_")))

(defn viml-fn-bridge [mod name opts]
  "(viml-fn-bridge *module-name* (sym->name some-func))
  Creates a global Vim function that will call a lua/fnl module function
  Given a module foo.bar-baz and a function do-thing, the global vim function name will be formated
  Foo_bar_baz_do_thing"
  (let [Mod (r.upper-first mod)
        func-name (viml-name-format (.. Mod "_" name "_viml"))]
    (nutils.fn-bridge func-name mod name opts)
    func-name))

(defn viml->lua [m f opts]
  "Creates a string that call a lua/fennel module function.
  (viml->lua :module.a :module-function {:args ['foo' 'bar']})"
  (..
    "lua require('" m "')"
    "['" f "']"
    "(" (or (and opts opts.args) "") ")"))

(defn cviml->lua [m f opts]
  "Creates a command appropriate string that call a lua/fennel module function.
  (cviml->lua :module.a :module-function {:args ['foo' 'bar']})"
  (.. "<CMD>" (viml->lua m f opts) "<CR>"))

(defn viml->luaexp [m f args]
  (.. "luaeval('require(\"" m "\")[\"" f "\"](" (or args "") ")')"))

(defn safe-cnoreabbrev [from to]
  "(safe-cnoreabbrev :foo :echo)
  ensures the command is not"
  (nvim.ex.cnoreabbrev
    (..
      "<expr> "
      from
      ; if cmd type is ex command and the command is <from>
      ; then return <to>
      ; otherwise return <from>
      " ((getcmdtype() is# ':' && getcmdline() is# '" from "')? "
      "('" to "') : "
      "('" from "'))")))


(defn get-cursor-pos []
  "(get-cursor-pos) => [x, y]
  get the chars under the cursor"
  [(nvim.fn.line ".")
   (nvim.fn.col ".")])

(defn get-char-under-curs []
  "(get-char-under-curs)
  get the character under the cursor
  should work with multi-byte chars but is slower than other methods"
  (let [line (nvim.fn.getline ".")
        col (nvim.fn.col ".")
        matchReg (.. "\\%" col "c.")]
    (nvim.fn.matchstr line matchReg)))

(defn set-nvim-g! [map]
  (assert (= (type map) "table") (.. "set-nvim-g! expects a table but got a: " (tostring map)))
  (->>
    map
    (r.to-pairs)
    (r.forEach
      (fn [[key val]] (tset nvim.g key val))))
  map)

(defn set-nvim-o! [map]
  (assert (= (type map) "table") (.. "set-nvim-0! expects a table but got a: " (tostring map)))
  (->>
    map
    (r.to-pairs)
    ; map to from to a string for ':set'
    (r.map
      (fn [[from to]]
        (if (r.boolean? to)
          ; booleans need to be translated
          ; true  = :set foo
          ; false = :set nofoo
          (if (r.true? to)
            from
            (.. "no" from))
          (.. from "=" to))))
    (r.forEach
      ; execute set as using the nvim api does not happen before the initial file is loaded
      (fn [arg] (nvim.ex.set arg))))
  map)

(def regex vim.regex)
