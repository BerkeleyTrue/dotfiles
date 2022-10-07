(module plugins.mumber
  {require
   {a aniseed.core
    r r
    md utils.module
    utils utils
    palette theme.palette}
   require-macros [macros]})

(comment (utils.get_hl_by_name :Visual true))

(defn- get-highlight-fg [group]
  (. (utils.get_hl_by_name group true) :foreground))

(def- modes
  {:n (palette.get-color-by-name :purple)
   :i (palette.get-color-by-name :green)
   :v (palette.get-color-by-name :yellow)
   :V (palette.get-color-by-name :yellow)
   "" (palette.get-color-by-name :purple)
   :s (palette.get-color-by-name :pink)
   :S (palette.get-color-by-name :pink)
   :R (palette.get-color-by-name :red)
   :c (palette.get-color-by-name :orange)})

(defn- get-highlight-for-mode [mode]
  (or (. modes mode) modes.n))

(comment (get-highlight-for-mode :n))

(defn- get-curr-mode []
  (. (utils.get_mode) :mode))

(comment (get-curr-mode))

(defn- set-highlight [color]
  (let [base-highlight (utils.get_hl_by_name :CursorLineNr true)
        opts (vim.tbl_extend :keep {:foreground color} base-highlight)]
    (utils.set_hl 0 :CursorLineNr opts)))

(defn handle-mode-changed []
  (->
    (get-curr-mode)
    (get-highlight-for-mode)
    (set-highlight)))

(defn main []
  (augroup
    :Mumber
    {:event [:ModeChanged :BufEnter :BufNew]
     :pattern :*
     :callback handle-mode-changed}))
