(module utils.keys
  {require
   {md utils.module
    utils utils}
   require-macros [macros]})

(defn feed [str noremap]
  (let [mode (if noremap :n :m)]
    (->
      str
      (utils.replace-termcodes)
      (utils.fn.feedkeys mode))))

(defn feed-noremap [str]
  (feed str true))

(comment
  (feed "i")
  (feed-noremap "i"))
