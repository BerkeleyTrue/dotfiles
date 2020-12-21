(module maps
  {:require {: r
             : utils}})

; common command aliases
(->>
  {:W :w
   :qq :q!
   :Wq :wq
   :sudowrite "w !sudo tee %"}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.safe-cnoreabbrev from to))))

(utils.set-nvim-g!
  {:mapleader ","
   :maplocalleader ","
   ; Turn of default <C-j> binding
   :BASH_Ctrl_j "off"})


; easy insert escape
(->>
  {:jj "<ESC>"
   :kk "<ESC>"}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.inoremap from to))))

; window nav
(->>
  {:<C-h> :<C-w>h
   :<C-j> :<C-w>j
   :<C-k> :<C-w>k
   :<C-l> :<C-w>l
   :<C-w>vh :<C-w>t<C-w>K
   :<C-w>hv :<C-w>t<C-w>H}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.nnoremap from to))))

; |-move to the end of the line in visual mode
(utils.vnoremap :L :g_)
