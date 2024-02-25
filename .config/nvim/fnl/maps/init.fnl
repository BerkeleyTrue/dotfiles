(module maps
  {autoload
   {a aniseed.core
    r r
    utils utils}
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
  {:jj :<ESC>jj
   :kk :<ESC>kk}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (inoremap from to))))

; ==<window navigati
(->>
  {:<C-h> :<C-w>h
   :<C-j> :<C-w>j
   :<C-k> :<C-w>k
   :<C-l> :<C-w>l
   :<C-w>vh :<C-w>t<C-w>K
   :<C-w>hv :<C-w>t<C-w>H}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (nnoremap from to))))

; ==<buffer navigation>==
(nnoremap :<leader>bn ":bnext<CR>" {:desc "go to next buffer" :silent true})
(nnoremap :<leader>bp ":bprevious<CR>" {:desc "go to previous buffer" :silent true})

; ==<tab navigation>==
(nnoremap :<leader>tn ":tabnext<CR>" {:desc "go to next tab" :silent true})
(nnoremap :<leader>tp ":tabprevious<CR>" {:desc "go to previous tab" :silent true})

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

; sort lines in visual mode
(vnoremap :<leader>s ":sort<cr>")

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
(nnoremap :Q "@qj")
; run a macro over a selection of lines
(xnoremap :Q ":normal @q")


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

(defn- preserve-cursor-loc [seq mark post]
  "Preserve the cursor location while running a seq, then return to location and delete mark"
  (assert (r.string? mark) (.. "preserve-cursor-loc expects a mark but received: " mark))
  (..
    ; make sure mark isn't already set
    "<ESC>:delmarks " mark "<CR>"
    ; set mark
    "m" mark
    ; do seq
    seq
    ; go back to mark
    "`q"
    ; delete mark
    "<ESC>:delmarks " mark "<CR>"
    (or post "")))

(nnoremap "z;" (preserve-paste (preserve-cursor-loc "A;<esc>" :q)) {:silent true})
; Throw a comma on the end of the line
(nnoremap "z," (preserve-paste (preserve-cursor-loc "A,<esc>" :q)) {:silent true})
; Delete last character on the line
(nnoremap :zdl (preserve-paste (preserve-cursor-loc "A<esc>x" :q)) {:silent true})
; Move the current char to the end of the line
(nnoremap :zl (preserve-paste (preserve-cursor-loc "x$p" :q)) {:silent true})
; Move line to the end of the next line
; useful for move a comment above a line behind it
(nnoremap :zJ (preserve-paste :ddpkJ) {:silent true})

(nnoremap :<Space> :za)
(vnoremap :<Space> :za)

; keeps freezing my vim??
(inoremap :<F10> :<nop>)

(nmap :<ScrollWheelUp> :k)
(nmap :<S-ScrollWheelUp> :k)
(nmap :<ScrollWheelDown> :j)
(nmap :<S-ScrollWheelDown> :j)

; captilize word under cursor
(nmap :gcw (preserve-cursor-loc "eb~" :q) {:silent true})
; TODO: preserve highlight
(vmap :gcw (preserve-cursor-loc "<esc>eb~`qv" :q "v") {:silent true})
