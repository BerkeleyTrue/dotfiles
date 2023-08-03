(module lib.lazy
  {require
   {a aniseed.core
    r r
    md utils.module
    nvim aniseed.nvim}
   require-macros [macros]})

(defn- get-dir []
  (string.format "%s/lazy/lazy.nvim" (nvim.fn.stdpath "config")))

(defn- does-file-exist [path]
  (match (io.open path :r)
    (nil msg) false
    f true))

(defn- gclone []
  (->
    (string.format
      "git clone %s %s %s %s %s"
      "https://github.com/folke/lazy.nvim.git"
      (get-dir)
      "--depth=1"
      "--filter=blob:none"
      "--branch=stable")
    (nvim.fn.system)))

(defn- ensure-exist []
  (let [exists? (vim.loop.fs_stat (get-dir))
        install? (if (not exists?)
                     (= (nvim.fn.input
                         {:prompt "lazy.nvim is missing. Would you like to download it?"
                          :default "y"})
                        "y")
                     false)]
    (when install?
      (nvim.echo "   Downloading lazy...   ")
      (nvim.echo (gclone)))))

(defn- format-plugins [plugin]
  (let [{:name name} plugin]
    (->
      plugin
      (r.assoc 1 name)
      (r.assoc :name nil)
      (r.assoc :description nil))))

(defn setup [spec]
  (let [rtp (vim.opt.rtp:get)
        nix-pack-dirs (r.filter #(string.find $ "vim-pack-dir" 1 true) rtp)]
    ; (print (vim.inspect nix-pack-dirs))
    (when-let [lazy (md.prequire :lazy)]
      (lazy.setup
        (r.map format-plugins spec)
        {:performance
         {:reset_packpath false
          :rtp
          {:paths (r.concat ["$HOME/.nix-profile/share/nvim/site"] nix-pack-dirs)}
          :reset true}}))))

(defn main [spec]
  (ensure-exist)
  (vim.opt.rtp:prepend (get-dir)))
