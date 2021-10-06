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

; Make OSX alt key compatible with vim
; Now alt will work as expected with vim-move
(->>
  {:˙ :<A-h>
   :∆ :<A-j>
   :˚ :<A-k>
   :¬ :<A-l>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.nmap from to))))

(->>
  {:˙ :<A-h>
   :∆ :<A-j>
   :˚ :<A-k>
   :¬ :<A-l>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.imap from to))))

(->>
  {:˙ :<A-h>
   :∆ :<A-j>
   :˚ :<A-k>
   :¬ :<A-l>}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.imap from to))))

; I keep hitting this on failed :q's
; Open commandline instead and wait for further commands
; use <C-f> while in command mode to access this instead
(utils.nnoremap :q: ":")

; sort lines in visual mode
(utils.vnoremap :<leader>s ::sort<cr>)

; insert new line on enter
(utils.nnoremap :<cr> :o<esc>)

; in visual mode use gu to change casing
(utils.vnoremap :u :<nop>)
(utils.vnoremap :gu :u)

; In normal mode remove ( text object motion. I keep hiting this accidentally
; and never use it intentially
(utils.nnoremap "(" :<nop>)
(utils.nnoremap ")" :<nop>)


; in normal mode, run quick macro
; use gQ to enter Exmode instead
(utils.nnoremap :Q "@q")


; yank to end of line
(utils.nnoremap :Y :y$)

; reselect visual block after indent
(utils.vnoremap :< :<gv)
(utils.vnoremap :> :>gv)

; reselect last paste
(utils.nnoremap :gp "'`[' . strpart(getregtype(), 0, 1) . '`]'" {:expr true})

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

(utils.nnoremap "z;" (preserve-paste (preserve-cursor-loc "A;<esc>`q" :q)))
; Throw a comma on the end of the line
(utils.nnoremap "z," (preserve-paste (preserve-cursor-loc "A,<esc>`q" :q)))
; Delete last character on the line
(utils.nnoremap :zdl (preserve-paste (preserve-cursor-loc "A<esc>x`q" :q)))
; Move the current char to the end of the line
(utils.nnoremap :zl (preserve-paste (preserve-cursor-loc "x$p`q" :q)))
; Move line to the end of the next line
; useful for move a comment above a line behind it
(utils.nnoremap :zJ (preserve-paste :ddpkJ))
; join line below removing surrounding whitespace
(utils.nnoremap :gJ "gJi <ESC>diW")

(utils.nnoremap :<Space> :za)
(utils.vnoremap :<Space> :za)

; keeps freezing my vim??
(utils.inoremap :<F10> :<nop>)

(utils.nmap :<ScrollWheelUp> :k)
(utils.nmap :<S-ScrollWheelUp> :k)
(utils.nmap :<ScrollWheelDown> :j)
(utils.nmap :<S-ScrollWheelDown> :j)
