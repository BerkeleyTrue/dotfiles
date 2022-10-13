(local module-sym (gensym))

(local M {})

; =<< utils >=>
(fn keys [t]
  "Get all keys of a table."
  (let [result []]
    (when t
      (each [k _ (pairs t)]
        (table.insert result k)))
    result))

(fn table? [x]
  "checks if 'x' is of table type."
  `(= :table (type ,x)))

(fn string? [val] (= (type val) :string))

(fn nil? [val] (= (type val) :nil))

(fn empty? [val]
  "(empty? {}) ;=> true
   (empty? []) ;=> true
   (empty? \"\") ;=> true"
  (if
    (table? val) (nil? (next val))
    (string? val) (= (length val) 0)
    (nil? val) true
    false))

(fn count [xs]
  (if
    (table? xs) (let [maxn (table.maxn xs)]
                  ;; We only count the keys if maxn returns 0.
                  (if (= 0 maxn)
                    (table.maxn (keys xs))
                    maxn))
    (not xs) 0
    (length xs)))

(fn run! [f xs]
  "Execute the function (for side effects) for every xs."
  (when xs
    (let [nxs (count xs)]
      (when (> nxs 0)
        (for [i 1 nxs]
          (f (. xs i)))))))

(fn reduce [f init xs]
  "Reduce xs into a result by passing each subsequent value into the fn with
  the previous value as the first arg. Starting with init."
  (var result init)
  (run!
    (fn [x]
      (set result (f result x)))
    xs)
  result)

(fn merge! [base ...]
  (reduce
    (fn [acc m]
      (when m
        (each [k v (pairs m)]
          (tset acc k v)))
      acc)
    (or base {})
    [...]))

(fn merge [...]
  (merge! {} ...))

(fn dolist [lst]
  "unpacks 'lst' and wrap it within do block."
  `(do ,(unpack lst)))

(lambda parse-sym [xs]
  "parses symbol 'xs' converts it to string if not a variable."
  (if (or (in-scope? xs) (not (sym? xs)))
    xs
    (tostring xs)))

; =<< module system >=>
(macro defn [name args ...]
  "defines an exported function 'name'."
  `(tset
    M
    ,(tostring name)
    (fn ,name ,args ,...)))

; TODO: move out of macro
(defn run-main [name ...]
  `(let [args# ,[...]]
     ; conditional threadfirst
     (-?>
       ,name

       ; pcall require
       ((fn [name#]
          (let [(ok# res#) (pcall require name#)]
            (if
              ok# res#
              (print (.. "Could not load module "
                         (tostring name#)
                         ": " (tostring res#)))))))
       ((fn [mod#]
          (assert
            (= (type mod#) "table")
            (.. "run-main expects a table to be exported by " (tostring ,name) " but found: " (tostring mod#)))
          (assert
            (= (type (. mod# :main)) "function")
            (.. "run-main expects a module with a public main function but found " (tostring (. mod# :main)) " for " (tostring ,name)))
          mod#))

       ((fn [mod#] ((. mod# :main) (unpack args#)))))))

(defn sym->name [a-sym]
  "Symbol to name as a string."
  (tostring a-sym))

(defn from-iter [iter-fact]
  "(from-iter (pairs {:apple \"red\" :orange \"orange\"})) ;=> {:red \"apple\" :orange \"orange\"}
   When vals are
   (from-iter (: \"foo bar\" :gmatch \"%S+\")) ;=> {:foo true :bar true}"
  `(let [tbl# {}]
     (each [k# v# ,iter-fact]
       (tset tbl# k# (if (= v# nil) true v#)))
     tbl#))

(defn from-seq-iter [iter-fact]
  "(from-seq-iter (ipairs [:apple :orange])) ;=> [:apple :orange]"
  `(let [tbl# []]
     (each [_# v# ,iter-fact]
       (table.insert tbl# v#))
     tbl#))

(defn when-let
  [bindings ...]
  (assert
     (= (type bindings) "table")
     (.. "expects a table for its binding but found : " (tostring bindings)))
  (assert
    (= 2 (table.maxn bindings)) "exactly 2 forms in binding vector")
  (let [form (. bindings 1)
        tst (. bindings 2)]
    `(let [temp# ,tst]
       (when temp#
         (let [,form temp#]
           ,...)))))

(defn when-not
  [test ...]
  (list (sym :if)
        (list (sym :not) test)
        (list (sym :do) ...)))

(defn if-let
  [bindings then else]
  (assert
     (= (type bindings) "table")
     (.. "expects a table for its binding but found : " (tostring bindings)))
  (assert
    (= 2 (table.maxn bindings)) "exactly 2 forms in binding vector")
  (let [form (. bindings 1)
        tst (. bindings 2)]
    `(let [temp# ,tst]
       (if temp#
         (let [,form temp#]
           ,then)
         ,else))))

(defn logx [x]
  "(logx foo) ;=> (print \"foo: \" foo)"
  `(print ,(.. (tostring x) ": ") ,x))


(defn viml->lua* [symb opts]
  `(..
     "lua require('" *module-name* "')"
     "['" ,(tostring symb) "']"
     "(" ,(or (and opts opts.args) "") ")"))

(defn cviml->lua* [symb opts]
  `(.. "<CMD>" (viml->lua* ,symb ,opts) "<CR>"))

(defn viml->luaexp* [symb args]
  `(.. "luaeval('require(\"" *module-name* "\")[\"" ,(tostring symb) "\"](" (or args "") ")')"))


(defn get-lua-filename []
  "Get the lua filename of the current file"
  `(->
    *file*
    (vim.fn.substitute "\\.fnl$" ".lua" "")
    (vim.fn.substitute "fnl/" "lua/" "")))

(defn make-on-load [ns]
  `(.. "require(\"plugins." ,(tostring ns) "\").main()"))


(defn parse-command-args [args]
  (local out {:force true})
  (each [key val (pairs (or args {}))]
    (tset out key val))
  out)

(defn command! [lhs rhs args]
  `(vim.api.nvim_create_user_command ,lhs ,rhs ,(parse-command-args args)))


; =<< variables >=>
(defn g [name]
  "gets global variable 'name'."
  `(. vim.g ,(parse-sym name)))

(defn g! [name val]
  "sets global variable 'name' to 'val'."
  `(tset vim.g ,(parse-sym name) ,val))

(defn b [name val]
  "sets buffer scoped variable 'name'."
  `(. vim.b ,(parse-sym name)))

(defn b! [name val]
  "sets buffer scoped variable 'name' to 'val'."
  `(tset vim.b ,(parse-sym name) ,val))

(defn o [name]
  "get the option value"
  `(. vim.o ,(parse-sym name)))

(defn o! [name val]
  "sets buffer scoped option 'name' to 'val'."
  `(tset vim.o ,(parse-sym name) ,val))

(defn v [name]
  "get the value of the variable 'name'"
  `(. vim.v ,(parse-sym name)))

; =<< execute >=>
(defn cmd [name def]
  "execute command 'name' with the structured Dictionary 'def'.
  if 'name' is a variable, it is used as cmd option to nvim_cmd,
  if name is a string or a sym, we parse out the bang."
  (let [var? (in-scope? name)
        cmd-string (if var? name (tostring name))
        (cmd bangs?) (if var? (values cmd-string 0) (string.gsub cmd-string "!" ""))
        bang? (>= bangs? 1)]
    `(vim.api.nvim_cmd ,(merge {:cmd cmd :bang bang?} def))))

(defn command [name ...]
  "execute command 'name' and concat with arguments"
  `(vim.api.nvim_command (table.concat (vim.tbl_flatten [,(tostring name) ,...]) " ")))

(defn n [name ...]
  "run vim.api.nvim_[n] api"
  (let [f (.. "vim.api.nvim_" (tostring name))]
    `(,f ,...)))

(defn vf [name ...]
  "run viml function 'name'"
  `(vim.api.nvim_call_function ,(tostring name) ,...))

; =<< augroup >=>
(fn autocmd [id config]
  (let [event config.event
        events (if (string? event) [event] event)]
    (tset config :event nil)
    (when
      config.cmd
      (do
        (tset config :command config.cmd)
        (tset config :cmd nil)))
    `(vim.api.nvim_create_autocmd ,events ,(merge config {:group id}))))

(defn augroup [name ...]
  "Create an autogroup 'name' of autocmds of args passed to augroup"
  (let [cmds [...]
        id (gensym :augid)
        out (reduce
              (fn [out cmd]
                (assert-compile
                  (and (table? cmd) (not (empty? cmd)))
                  (.. " augroup expects autocommands to be tables, but found: " (view cmd))
                  cmd)
                (table.insert out (autocmd id cmd))
                out)
              [`(local ,id (vim.api.nvim_create_augroup ,name {:clear true}))]
              cmds)]
    (dolist out)))

:return M
