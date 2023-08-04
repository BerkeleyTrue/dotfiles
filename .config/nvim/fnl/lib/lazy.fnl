(module lib.lazy
  {require
   {a aniseed.core
    r r
    md utils.module
    nvim aniseed.nvim}
   require-macros [macros]})

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
          {:paths (r.concat ["$HOME/.nix-profile/share/nvim/site"] nix-pack-dirs) :reset true}}}))))

(defn main [])
