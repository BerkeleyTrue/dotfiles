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

(fn even? [n]
  (= (% n 2) 0))

(fn odd? [n]
  (not (even? n)))

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

(fn assoc [t ...]
  (let [[k v & xs] [...]
        rem (count xs)
        t (or t {})]

    (when (odd? rem)
      (error "assoc expects even number of arguments after table, found odd number"))

    (when (not (nil? k))
      (tset t k v))

    (when (> rem 0)
      (assoc t (unpack xs)))

    t))

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

;; =<< conditional-let >=>

(fn conditional-let-out [{: bind-expr
                          : bind-expr-cond
                          : value-expr
                          : branch
                          : body
                          : rest-bindings}]

  `(let [,bind-expr ,value-expr]
     (,branch ,(or bind-expr-cond bind-expr)
        ,(if (> (table.maxn (or rest-bindings [])) 0)
           `(let ,rest-bindings
              ,(unpack body))
           (unpack body)))))

; based on aniseed.macros.conditional-let
; extended to allow extra non-conditional bindings in one table
(fn conditional-let [branch bindings body]
  (assert
    (table? bindings)
    (.. "expects a table for its binding but found : " (tostring bindings)))
  (assert
    (= 0 (math.fmod (table.maxn bindings) 2))
    (.. "expected even number of bindings but found " (table.maxn bindings)) " bindings")

  (let [[bind-expr value-expr & rest-bindings] bindings
        extra-bindings? (> (table.maxn rest-bindings) 0)]
    (if
      ;; Simple symbols
      ;; [foo bar]
      (sym? bind-expr)
      (conditional-let-out {: bind-expr
                            : value-expr
                            : branch
                            : rest-bindings
                            : body})

      ;; List / values destructure
      ;; [(a b) c]
      (list? bind-expr)
      (do
        ;; Even if the user isn't using the first slot, we will.
        ;; [(_ val) (pcall #:foo)]
        ;;  => [(bindGENSYM12345 val) (pcall #:foo)]
        (when (= :_ (tostring (. bind-expr 1)))
          (tset bind-expr 1 (gensym "bind")))
        (conditional-let-out {: bind-expr
                              : value-expr
                              : branch
                              : rest-bindings
                              :bind-expr-cond (. bind-expr 1)
                              : body}))


      ;; Sequential and associative table destructure
      ;; [[a b] c]
      ;; [{: a : b} c]
      (table? bind-expr)
      `(let [value# ,value-expr
             ,bind-expr (or value# {})]
         (,branch value# ,(unpack body)))

      ;; We should never get here, but just in case.
      (assert (.. "unknown bind-expr type: " (type bind-expr))))))

(defn when-let [bindings & body]
  "when let macro. It is a combination of when and let.
  It is used to bind a value to a name and then execute a block of code if the value is not nil.
  (when-let [x (get-value)] (print x))
  (when-let [x (get-value)
             ; only the first is used for the condition
             ; the rest around bound after the condition passes
             y (get-value)]
    (print x y))
  (when-let [(ok? x) (get-value)]
            ; a (boolean, any) tuple is used for the condition,
            ; ok? must be true
    (print ok? x))"
  (conditional-let (sym :when) bindings body))

(defn if-let [bindings & body]
  "if-let macro. It is a combination of if and let.
  It is used to bind a value to a name and then execute a block of code if the value is not nil.
  (if-let [x (get-value)]
    (print x)
    (print 'x is nil')
  (if-let [x (get-value)
             ; only the first is used for the condition
             ; the rest around bound after the condition passes
             y (get-value)]
    (print x y)
    (print 'x is nil')
  (if-let [(ok? x) (get-value)]
            ; a (boolean, any) tuple is used for the condition,
            ; ok? must be true, and x must exits (non-nil)
    (print ok? x))"
  (assert (= (length body) 2) "if-let expects exactly 2 branches")
  (conditional-let (sym :if) bindings body))

(defn when-not [test ...]
  `(when (not ,test)
     ,...))

(defn if-not [test ...]
  `(if (not ,test)
     ,...))

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
  "Make a function that loads the main function of a module, lazily."
  `(fn [] ((. (require ,(.. "plugins." (tostring ns))) :main))))

(defn make-init [ns]
  "Make a function that loads the init function of a module, eagerly."
  `(. (require ,(.. "plugins." (tostring ns))) :init))

(defn parse-command-args [args]
  (local out {:force true})
  (each [key val (pairs (or args {}))]
    (when (not (= (tostring key) :buffer))
      (tset out key val)))
  out)

(defn command! [lhs rhs args]
  (let [args (or args {})
        buffer (if (nil? (. args :buffer)) 
                 nil 
                 (. args :buffer))
        command (if buffer 
                  `(vim.api.nvim_buf_create_user_command ,buffer)
                  `(vim.api.nvim_create_user_command))
        parsed-args (parse-command-args args)]
    (table.insert command lhs)
    (table.insert command rhs)
    (table.insert command parsed-args)
    command))

; =<< variables >=>
(defn g [name]
  "gets global variable 'name'."
  `(. vim.g ,(parse-sym name)))

(defn g! [name val]
  "sets global variable 'name' to 'val'."
  `(tset vim.g ,(parse-sym name) ,val))

(defn b [name]
  "gets buffer scoped variable 'name'."
  `(. vim.b ,(parse-sym name)))

(defn b! [name val]
  "sets buffer scoped variable 'name' to 'val'."
  `(tset vim.b ,(parse-sym name) ,val))

(defn o [name]
  "get the option value"
  `(. vim.o ,(parse-sym name)))

(defn o! [name val]
  "sets scoped option 'name' to 'val'."
  `(tset vim.o ,(parse-sym name) ,val))

(defn bo [name bufnr]
  "get the buffer option value"
  (if bufnr
    `(. vim.bo ,bufnr ,(parse-sym name))
    `(. vim.bo ,(parse-sym name))))

(defn bo! [name val bufnr]
  "sets buffer scoped option 'name' to 'val'."
  (if bufnr
    `(tset vim.bo ,bufnr ,(parse-sym name) ,val)
    `(tset vim.bo ,(parse-sym name) ,val)))

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
    `(vim.api.nvim_cmd ,(merge {:cmd cmd :bang bang?} def) {})))

(defn command [name ...]
  "execute command 'name' and concat with arguments
   approximately `:,name {...}.join(" ")`
  "
  `(vim.api.nvim_command (table.concat (vim.tbl_flatten [,(tostring name) ,...]) " ")))

(defn echo [msg history? opts?]
  "macro to nvim_echo but with better defaults"
  `(vim.api.nvim_echo ,msg ,(or history? true) ,(or opts? {})))

(defn echoerr [errstring]
  "Echo error message"
  `(vim.api.nvim_command (.. "echoerr \"" ,errstring "\"")))

(defn n [name ...]
  "run vim.api.nvim_[n] api"
  (let [name (string.gsub (tostring name) "-" "_")
        f (.. "vim.api.nvim_" name)]
    `(,(sym f) ,...)))

(defn vf [name ...]
  "run vim.fn.[name]"
  (let [fnname (.. "vim.fn." (tostring name))]
    `(,(sym fnname) ,...)))

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


; =<< keymap  >>=>

(local map-types
  {:n :n
   :o :o
   :v :v
   :i :i
   :x :x
   :s :s
   :c :c
   :a ""})

(fn base-map [mtype lhs rhs options]
  "(base-map :n :<CR> ':echo foo' {:expr true :buffer false :nowait false})"
  (let [mode (. map-types mtype)]
    `(vim.keymap.set ,mode ,lhs ,rhs ,(or options {}))))

(defn nmap [lhs rhs options] (base-map :n lhs rhs options))
(defn amap [lhs rhs options] (base-map :n lhs rhs options))
(defn omap [lhs rhs options] (base-map :o lhs rhs options))
(defn imap [lhs rhs options] (base-map :i lhs rhs options))
(defn smap [lhs rhs options] (base-map :s lhs rhs options))
(defn vmap [lhs rhs options] (base-map :v lhs rhs options))
(defn xmap [lhs rhs options] (base-map :x lhs rhs options))
(defn cmap [lhs rhs options] (base-map :c lhs rhs options))

(defn noremap [lhs rhs options?]
  "norecur map for all modes"
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :a lhs rhs options)))

(defn nnoremap [lhs rhs options?]
  "(nnoremap 'cr' ':echo foo' {:expr true :buffer false :nowait false})
  create a nnoremap"
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :n lhs rhs options)))

(defn inoremap [lhs rhs options?]
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :i lhs rhs options)))

(defn vnoremap [lhs rhs options?]
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :v lhs rhs options)))

(defn xnoremap [lhs rhs options?]
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :x lhs rhs options)))

(defn snoremap [lhs rhs options?]
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :s lhs rhs options)))

(defn cnoremap [lhs rhs options?]
  (let [options (assoc (or options? {}) :noremap true)]
    (base-map :c lhs rhs options)))

(defn set-hl [name opts]
  `(vim.api.nvim_set_hl 0 ,name ,opts))

:return M
