(module plugins.packer
  {:require {a aniseed.core
             nvim aniseed.nvim}})

(defn- get-packer-dir []
  (string.format "%s/pack/plugins/opt" (nvim.fn.stdpath "config")))

(defn- make-dir []
  (nvim.fn.mkdir (get-packer-dir) "p"))

(defn- does-file-exist [path]
  (match (io.open path :r)
    (nil msg) false
    f true))

(defn- gclone-packer []
  (-> (string.format "git clone %s %s%s" "https://github.com/wbthomason/packer.nvim " (get-packer-dir) "/packer.nvim")
      (nvim.fn.system)))


(defn- ensure-packer-exist []
  (let [packer-exists?  (does-file-exist (.. (get-packer-dir) "/packer.nvim"))
        install-packer? (if
                          (not packer-exists?) (= (nvim.fn.input {:prompt "Packer.nvim is missing. Would you like to download it?"
                                                                  :default "y"})
                                                  "y")
                          false)]
    (when install-packer?
      (-> (get-packer-dir)
          (make-dir))
      (nvim.echo "   Downloading packer...   ")
      (nvim.echo (gclone-packer)))))

(defn- packadd [] (nvim.ex.packadd :packer.nvim))

(defn- format-plugins [plugin]
  (let [{:name name} plugin]
    (-> plugin
        (a.assoc 1 name))))

(defn- create-planery-win []
  (pcall (fn []
           (let [plenary (require "plenary.window.float")]
             (plenary.percent_range_window 0.8 0.8)))))

(defn- open-fn [name]
  (let [(ok float-win) create-planery-win]
    (if
      (not ok) (do
                 (vim.cmd "65vnew [packer]")
                 (values (nvim.get_current_win) (nvim.get_current_buf)))
      (let [bufnr (a.get float-win :bufnr)
            win (a.get float-win :win_id)]
        (nvim.buf_set_name bufnr name)
        (nvim.buf_set_option win "winblend" 10)))))


(defn config [spec]
  (let [packer (require :packer)]
    (packer.startup
      {1 (fn [use]
           (->> spec
                (a.map format-plugins)
                (a.map use)))

       :config {:package_root (.. (nvim.fn.stdpath "config") "/pack")
                :open_fn open-fn}})))

(do
  (ensure-packer-exist)
  (packadd)
  {:config config})
