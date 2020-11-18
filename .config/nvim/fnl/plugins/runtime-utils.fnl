(module plugins.runtime-utils
  {:require {a aniseed.core
             nvim aniseed.nvim
             nutils aniseed.nvim.util
             utils utils}}
  {:get-hi-info get-hi-info})


(defn- get-syn-id [xys opaque?]
  (let [t? (if opaque? 1 0)
        line (a.first xys)
        col (a.second xys)]
    (nvim.fn.synID line col t?)))

(defn get-symbol [syn-id]
  (nvim.fn.synIDattr syn-id "name"))

(defn get-highlight []
  (->
    (utils.get-cursor-pos)
    (get-syn-id false)
    (get-symbol)))

(defn get-trans-highlight []
  (->
    (utils.get-cursor-pos)
    (get-syn-id true)
    (get-symbol)))

(defn get-highlight-group []
  (->
    (utils.get-cursor-pos)
    (get-syn-id true)
    (nvim.fn.synIDtrans)
    (get-symbol)))

(defn get-hi-info []
  (nvim.echo (..
              "hi<" (or (get-highlight) "none") "> "
              "trans<" (get-trans-highlight) "> "
              "group<" (get-highlight-group) "> ")))

(nutils.fn-bridge :GetHiInfo :plugins.runtime-utils :get-hi-info)

(utils.nnoremap "gh" ":call GetHiInfo()<CR>" {:silent true})

{:get-hi-info get-hi-info}
