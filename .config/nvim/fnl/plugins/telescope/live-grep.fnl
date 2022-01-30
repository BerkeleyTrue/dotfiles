(module plugins.telescope.live-grep
  {require
   {a aniseed.core
    r r
    md utils.module
    : utils}
   require-macros [macros]})

(defn live-grep [...]
  "Live grep search term."
  (if-let [builtins (md.prequire :telescope.builtin)]
    (let [query (r.join " " [...])
          opts {}]

      (when query (tset opts :default_text query))
      (builtins.live_grep opts))
    (vim.notify "no telescope.builtin was found")))

(defn live-grep-from-glob [...]
  "Live grep from a search term and a glob>
  (live-grep-glob \"a\" \"search\" \"string\" \"**/*.js\")"
  (if-let [builtins (md.prequire :telescope.builtin)]
    (let [args [...]
          n (length args)
          dirs (when (>= n 2)
                 (->> args
                   (a.last)
                   (#(utils.fn.glob $ 0 1))
                   (r.map (fn [path] (utils.fn.fnamemodify path ":p:h")))
                   (r.uniq)))

          query (->> args
                  ((if (= n 1) a.identity a.butlast))
                  (r.join " "))

          opts {}]

      (when query (tset opts :default_text query))
      (when dirs (tset opts :search_dirs dirs))
      (builtins.live_grep opts))
    (vim.notify "no telescope.builtin was found")))

(defn live-grep-from-cbd [...]
  "Live grep from the current buffer directory.
  (live-grep-from-cbd \"Foo bar\")"
  (if-let [builtins (md.prequire :telescope.builtin)]
    (let [path (utils.fn.expand "%:p:h")
          query (r.join " " [...])]
      (builtins.live_grep
        {:default_text query :cwd path}))
    (vim.notify "no telescope.builtin was found")))

(defn main [{: builtins}]
  (utils.ex.command_
    "-nargs=*"
    :Ag
    (utils.viml->lua *module-name* (sym->name live-grep) {:args "<f-args>"}))
  (utils.ex.command_
    "-nargs=*"
    :Agdot
    (utils.viml->lua *module-name* (sym->name live-grep-from-cbd) {:args "<f-args>"}))
  (utils.ex.command_
    "-nargs=*"
    :Agglob
    (utils.viml->lua *module-name* (sym->name live-grep-from-glob) {:args "<f-args>"})))
