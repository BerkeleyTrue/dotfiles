(module utils.keys
  {require
   {md utils.module
    utils utils}
   require-macros [macros]})

(defn replace-termcodes [str]
  (vim.api.nvim_replace_termcodes str true true true))

(comment (replace-termcodes "<CR>"))

(defn feed [str noremap]
  (let [mode (if noremap :n :m)]
    (->
      str
      (replace-termcodes)
      (vim.fn.feedkeys mode))))

(comment (feed "<CR>"))

(defn feed-noremap [str]
  (feed str true))

(comment
  (feed "i")
  (feed-noremap "i"))
