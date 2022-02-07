(module plugins.packer
  {require
   {a aniseed.core
    nvim aniseed.nvim}})

(defn- get-packer-dir []
  (string.format "%s/pack/packer/opt" (nvim.fn.stdpath "config")))

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

(defn- packadd [] (pcall nvim.ex.packadd :packer.nvim))

(defn- format-plugins [plugin]
  (let [{:name name} plugin]
    (-> plugin
        (a.assoc 1 name))))

(defn config [spec]
  (let [packer (require :packer)]
    (packer.startup
      {1 (fn [use]
           (->> spec
                (a.map format-plugins)
                (a.map use)))

       :config
       {:display
        {:open_fn
         (fn []
           ((. (require "packer.util") :float) {:border "rounded"}))}
        :package_root (.. (nvim.fn.stdpath "config") "/pack")}})))

(do
  (ensure-packer-exist)
  (packadd)
  {:config config})
