(module plugins.oil.lsp
  {autoload
   {a aniseed.core
    r r
    oil oil
    lsp oil-lsp-diagnostics}
   require {}
   import-macros []
   require-macros [macros]})


(def- namespace (vim.api.nvim_create_namespace :oil-lsp))
(def- config {:error ["  " :BerksError]
              :warn ["  " :BerksWarn]
              :info ["  " :DiagnosticInfo]
              :hint [" 󰌶 " :DiagnosticHint]})

(defn get-virt-text [severity]
  (. config severity))

(comment
  (get-virt-text :error)
  (get-virt-text :warn)
  (get-virt-text :info)
  (get-virt-text :hint)
  (get-virt-text :foo))

(defn get-buf-from-path [path]
  (->> (vim.api.nvim_list_bufs)
       (r.find 
         (fn [buf] 
           (let [name (vim.api.nvim_buf_get_name buf)]
             (= name path))))))

(comment
  (get-buf-from-path (vim.fn.expand "%:p")))

(defn get-diagnostics-summary [buffer-or-dir is-dir]
  (->> (vim.api.nvim_list_bufs)
       ; filter to only buffers that match the file or are in the dir
       (r.filter (fn [buf] (if is-dir 
                             (vim.startswith (vim.api.nvim_buf_get_name buf) buffer-or-dir) 
                             (= buf buffer-or-dir))))
        ; get diagnostics for each buffer
       (r.map #(vim.diagnostic.get $))
       (r.flatten)
       (r.group-by #(. vim.diagnostic.severity (. $ :severity)))
       (r.to-pairs)
       (r.map (fn [[key vals]] [(r.to-lower-case key) (r.size vals)]))
       (r.from-pairs)))

(comment
  (vim.diagnostic.set namespace
                       (get-buf-from-path (vim.fn.expand "%:p"))
                       [{:lnum 0 :col 0 :message "foo" :severity vim.diagnostic.severity.ERROR}
                        {:lnum 1 :col 10 :message "bar" :severity vim.diagnostic.severity.WARN}
                        {:lnum 2 :col 6 :message "baz" :severity vim.diagnostic.severity.INFO}
                        {:lnum 3 :col 2 :message "faz" :severity vim.diagnostic.severity.HINT}])
  (vim.diagnostic.get (get-buf-from-path (vim.fn.expand "%:p")))
  (get-diagnostics-summary (get-buf-from-path (vim.fn.expand "%:p")) false))

(defn add-lsp-extmarks [buffer]
  ; clear old marks
  (vim.api.nvim_buf_clear_namespace buffer namespace 0 (- 1))

  (->> (r.range 1 (vim.api.nvim_buf_line_count buffer))
       (r.map (fn [lnum]
                (let [dir (oil.get_current_dir buffer)
                      entry (oil.get_entry_on_line buffer lnum)
                      is-dir (or (and entry (= entry.type :directory)) false)
                      diagnostics (when entry
                                    (if is-dir
                                        (get-diagnostics-summary (.. dir entry.name "/") true)
                                        (let [file-buf (or (and entry 
                                                                (get-buf-from-path (.. dir entry.name)))
                                                           nil)
                                              is-active (or (and file-buf
                                                                 (vim.api.nvim_buf_is_loaded file-buf))
                                                            false)
                                              buffer-or-dir (if is-active file-buf (.. dir entry.name))]
                                          (get-diagnostics-summary buffer-or-dir (not is-active)))))]
                  [lnum diagnostics])))
       (r.filter (fn [[_ diagnostics]] diagnostics))
       (r.for-each (fn [[lnum diagnostics]]
                     (->> (r.to-pairs diagnostics)
                          (r.filter (fn [[_ count]] (> count 0)))
                          (r.reduce (fn [[acc priority] [sev count]] 
                                      [(r.conj acc [sev count priority]) 
                                       (+ priority 1)]) 
                                    [[] 0])

                          (r.head)
                          (r.for-each (fn [[severity _] priority]
                                        (vim.api.nvim_buf_set_extmark 
                                          buffer 
                                          namespace 
                                          (- lnum 1) 
                                          0
                                          {:priority priority
                                           :virt_text [(get-virt-text severity)]
                                           :hl_mode :combine}))))))))

(defn main []
  (augroup :OilLsp
    {:event :FileType
     :pattern :oil
     :callback 
     (r.void
       (fn oil-lsp-attach []
         (let [buffer (vim.api.nvim_get_current_buf)]
           (when-not (. (b buffer) :oil_lsp_started)
             (b! buffer :oil_lsp_started true)
             (augroup :OilLspAttached 
                {:event [:BufReadPost
                         :BufWritePost
                         :InsertLeave
                         :TextChanged]
                 :buffer buffer
                 :callback 
                 (r.void
                   (fn oil-add-marks []
                     (add-lsp-extmarks buffer)))})))))}))
