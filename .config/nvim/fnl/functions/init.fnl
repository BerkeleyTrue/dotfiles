(module functions
  {require
   {: r
    : utils}
   require-macros [macros]})


(defn buff-delete-hidden []
  (let [buffers (->>
                  ; how many pages are there
                  (utils.fn.tabpagenr "$")
                  ; create an array of n (num of pages)
                  (r.range 1)
                  ; map page to buf list for that page
                  (r.map #(utils.fn.tabpagebuflist $1))
                  ; flatten
                  (r.reduce r.concat []))
        to-delete (->>
                    (utils.fn.bufnr "$")
                    ; creates a range of numbers up to the largest buffer number
                    (r.range 1)
                    ; buffer actually exists
                    (r.filter #(= (utils.fn.bufexists $1) 1))
                    ; buffer is not in the list above
                    (r.reject #(r.some (r.is-equal $1) buffers))
                    ; filter for normal buffers
                    (r.filter #(= (utils.fn.empty (utils.fn.getbufvar $1 "&buftype")) 1))
                    ; filter for unmodified buffers
                    (r.filter #(= (utils.fn.getbufvar $1 "&mod") 0)))]
    (when (not (r.empty? to-delete))
       (r.for-each #(utils.ex.bwipeout $1) to-delete)
       (utils.print (.. "Closed " (r.size to-delete) " hidden buffers")))))


(utils.ex.command_ :BufDeleteHidden (utils.viml->lua *module-name* (sym->name buff-delete-hidden)))
