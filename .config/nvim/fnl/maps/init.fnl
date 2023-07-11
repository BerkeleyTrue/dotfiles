(module maps
  {require
   {: r
    : utils}
   require-macros [macros]})

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
  {:jj :<ESC>
   :kk :<ESC>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (inoremap from to))))

; window nav
(->>
  {:<C-h> :<C-w>h
   :<C-j> :<C-w>j
   :<C-k> :<C-w>k
   :<C-l> :<C-w>l
   :<C-w>vh :<C-w>t<C-w>K
   :<C-w>hv :<C-w>t<C-w>H}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (nnoremap from to))))

; |-move to the end of the line in visual mode
(vnoremap :L :g_)

; Make OSX alt key compatible with vim
; Now alt will work as expected with vim-move
(->>
  {:˙ :<A-h>
   :∆ :<A-j>
   :˚ :<A-k>
   :¬ :<A-l>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (nmap from to))))

(->>
  {:˙ :<A-h>
   :∆ :<A-j>
   :˚ :<A-k>
   :¬ :<A-l>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (imap from to))))

(->>
  {:˙ :<A-h>
   :∆ :<A-j>
   :˚ :<A-k>
   :¬ :<A-l>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (imap from to))))

; I keep hitting this on failed :q's
; Open commandline instead and wait for further commands
; use <C-f> while in command mode to access this instead
(nnoremap :q: ":")

; sort lines in visual mode
(vnoremap :<leader>s ::sort<cr>)

; insert new line on enter
(nnoremap :<cr> :o<esc>)

; in visual mode use gu to change casing
(vnoremap :u :<nop>)
(vnoremap :gu :u)

; In normal mode remove ( text object motion. I keep hiting this accidentally
; and never use it intentially
(nnoremap "(" :<nop>)
(nnoremap ")" :<nop>)


; in normal mode, run quick macro
; use gQ to enter Exmode instead
(nnoremap :Q "@q")


; yank to end of line
(nnoremap :Y :y$)

; reselect visual block after indent
(vnoremap :< :<gv)
(vnoremap :> :>gv)

; reselect last paste
(nnoremap :gp "'`[' . strpart(getregtype(), 0, 1) . '`]'" {:expr true})

(defn- preserve-paste [seq]
  (..
    ; store the last copied item into register z
    "<ESC>:let @z=@\"<CR>"
    ; do the thing
    seq
    ; restore last paste from z
    "<ESC>:let @\"=@z<CR>"))

(defn- preserve-cursor-loc [seq mark]
  (assert (r.string? mark) (.. "preserve-cursor-loc expects a mark but received: " mark))
  (..
    ; make sure mark isn't already set
    "<ESC>:delmarks " mark "<CR>"
    ; set mark
    "m" mark
    ; do seq
    seq
    ; delete mark
    "<ESC>:delmarks " mark "<CR>"))

(nnoremap "z;" (preserve-paste (preserve-cursor-loc "A;<esc>`q" :q)) {:silent true})
; Throw a comma on the end of the line
(nnoremap "z," (preserve-paste (preserve-cursor-loc "A,<esc>`q" :q)) {:silent true})
; Delete last character on the line
(nnoremap :zdl (preserve-paste (preserve-cursor-loc "A<esc>x`q" :q)) {:silent true})
; Move the current char to the end of the line
(nnoremap :zl (preserve-paste (preserve-cursor-loc "x$p`q" :q)) {:silent true})
; Move line to the end of the next line
; useful for move a comment above a line behind it
(nnoremap :zJ (preserve-paste :ddpkJ) {:silent true})
; join line below removing surrounding whitespace
(nnoremap :gJ "gJi <ESC>diW" {:silent true})

(nnoremap :<Space> :za)
(vnoremap :<Space> :za)

; keeps freezing my vim??
(inoremap :<F10> :<nop>)

(nmap :<ScrollWheelUp> :k)
(nmap :<S-ScrollWheelUp> :k)
(nmap :<ScrollWheelDown> :j)
(nmap :<S-ScrollWheelDown> :j)
