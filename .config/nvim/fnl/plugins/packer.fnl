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

(def plugins
  [{:name "wbthomason/packer.nvim"
    :opt true}
   {:name "nvim-lua/telescope.nvim"
    :requires [["nvim-lua/popup.nvim"] ["nvim-lua/plenary.nvim"]]}])


(defn packer-startup [use]
  (->> plugins
      (a.map format-plugins)
      (a.map use)))

(do
  (ensure-packer-exist)
  (packadd)
  (let [packer (require :packer)]
    (packer.startup
      {1 packer-startup
       :config {:package_root (.. (nvim.fn.stdpath "config") "/pack")}})))
