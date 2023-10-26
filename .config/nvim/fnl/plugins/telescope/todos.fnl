(module plugins.telescope.todos
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils

    pickers telescope.pickers
    builtins telescope.builtin
    finders telescope.finders
    config  telescope.config
    actions telescope.actions
    astate  telescope.actions.state
    make-entry telescope.make_entry
    tutils  telescope.utils}

   require-macros [macros]})

(def- keywords
  {:FIX  {:alts [:FIXME :BUG :FIXIT :ISSUE] :icon " " :hl :BerksError}
   :NOTE {:alts [:INFO] :icon " " :hl :BerksTLNote}
   :PERF {:alts [:OPTIM :PERFORMANCE :OPTIMIZE] :icon " " :hl :BerksTLPerf}
   :TEST {:alts [:TESTING :PASSED :FAILED] :icon "⏲ "}
   :WARN {:alts [:WARNING :XXX] :icon " " :hl :BerksWarn}
   :HACK {:icon " " :hl :BerksTLHack}
   :TODO {:icon " " :hl :BerksTLTodo}})

(def- keyword-map
  (->>
    keywords
    (r.to-pairs)
    (r.reduce
      (fn [acc [k val]]
        (let [alts (. val :alt)
              acc  (r.assoc acc k k)]
          (r.reduce (fn [acc alt] (r.assoc acc alt k)) acc alts)))
      {})))

(def- max-line-len 200)

(def- keywords-regex
  (..
    "\\b("
    (->>
      keywords
      (vim.tbl_keys)
      (r.join "|"))
    "):"))

(def- hl-regex
  (..
    ".*<("
    (->>
      keywords
      (vim.tbl_keys)
      (r.join "|"))
    ")\\s*:"))

(defn highlight-match [str]
  "Returns the start and end of the match, and the keyword that was matched."
  (when (< (length str) max-line-len)
    (let [m (vim.fn.matchlist str (.. "\\v\\C" hl-regex))]
      (when (and (> (length m) 1) (. m 2))
        (let [keyword (. m 2)
              start (str:find keyword)]
          (values start (+ start (length keyword)) keyword))))))

(defn make-display [entry]
  "Returns the display string and the highlight groups if a match is found."
  (let [display (string.format
                  "%s:%s:%s "
                  entry.filename
                  entry.lnum
                  entry.col)
        text entry.text
        (start finish kw) (highlight-match text)]
    (if start
      (let [kw (or (. keyword-map kw) kw)
            icon (. (. keywords kw) :icon)
            fg (or (. (. keywords kw) :hl) :Identifier)
            bg (if-let [hl (. (. keywords kw) :hl)] (.. hl :Inverse) :Identifier)
            display (.. icon " " display)
            text (vim.trim (text:sub start))
            hl-icon [[1 (+ (length icon) 1)]
                     fg]
            hl-display-bg [[(length display)
                            (-> display (length) (+ finish) (- start) (+ 2))]
                           bg]
            hl-display-fg [[(-> display (length) (+ finish) (- start) (+ 1))
                            (-> display (length) (+ finish) (+ 1) (+ (length text)))]
                           fg]]
        (values (.. display " " text) [hl-icon hl-display-bg hl-display-fg]))
      display)))

(defn todo []
  "Searches for TODOs in the current project and displays them in a picker."
  (let [opts {:prompt_title :Todos
              :use_regex true
              :search keywords-regex
              :vimgrep_arguments [:ag :--nocolor :--column :--noheading :--vimgrep]}
        entry-maker (make-entry.gen_from_vimgrep opts)]
    (set
      opts.entry_maker
      (fn [line]
        (let [ret (entry-maker line)]
          (set ret.display make-display)
          ret)))

    (builtins.grep_string opts)))


(defn main []
  (command! :Todos todo))
