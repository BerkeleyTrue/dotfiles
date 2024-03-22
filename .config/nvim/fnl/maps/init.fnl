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
   :Wq :wq}
   ; :sudowrite "w !sudo tee %" ; this doesn't work in neovim
   ; see https://github.com/neovim/neovim/issues/12103 for alts
  (r.to-pairs)
  (r.for-each (fn [[from to]] (utils.safe-cnoreabbrev from to))))

(g! mapleader ",")
(g! maplocalleader ",")
; Turn of default <C-j> binding
(g! :BASH_Ctrl_j "off")


; easy insert escape
(->>
  {:jj :<ESC>jj
   :kk :<ESC>kk}
  (r.to-pairs)
  (r.for-each (fn [[from to]] (inoremap from to))))

; ==<window navigation>==
(nnoremap :<C-h> :<C-w>h {:desc "move to window on the left"})
(nnoremap :<C-j> :<C-w>j {:desc "move to window below"})
(nnoremap :<C-k> :<C-w>k {:desc "move to window above"})
(nnoremap :<C-l> :<C-w>l {:desc "move to window on the right"})
(nnoremap :<C-w>vh :<C-w>t<C-w>K {:desc "split window vertically and move to it"})
(nnoremap :<C-w>hv :<C-w>t<C-w>H {:desc "split window horizontally and move to it"})

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
(vnoremap :<leader>s ":sort<cr>" {:desc "sort selection linewise"})

; insert new line on enter
(nnoremap :<cr> :o<esc>)

; in visual mode use gu to change casing
(vnoremap :u :<nop>)
(vnoremap :gu :u {:desc "lowercase selection"})

; In normal mode remove ( text object motion. I keep hiting this accidentally
; and never use it intentially
; (nnoremap "(" :<nop>)
; (nnoremap ")" :<nop>)


; in normal mode, run quick macro
; use gQ to enter Exmode instead
(nnoremap :Q "@qj" {:desc "run macro and move down one line"})
(xnoremap :Q ":normal @q" {:desc "run macro over selection"})

(nnoremap :Y :y$ {:desc "yank to end of line"})

(vnoremap :< :<gv {:desc "reselect visual block after dedent"})
(vnoremap :> :>gv {:desc "reselect visual block after indent"})

(nnoremap :gp "'`[' . strpart(getregtype(), 0, 1) . '`]'" {:expr true :desc "reselect last paste"})

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

(nnoremap "z;" (preserve-paste (preserve-cursor-loc "A;<esc>" :q)) {:silent true
                                                                    :desc "Add semicolon to end of line"})
; Throw a comma on the end of the line
(nnoremap "z," (preserve-paste (preserve-cursor-loc "A,<esc>" :q)) {:silent true
                                                                    :desc "Add comma to end of line"})
; Delete last character on the line
(nnoremap :zdl (preserve-paste (preserve-cursor-loc "A<esc>x" :q)) {:silent true
                                                                    :desc "Delete last character on the line"})
; Move the current char to the end of the line
(nnoremap :zl (preserve-paste (preserve-cursor-loc "x$p" :q)) {:silent true
                                                               :desc "Move the current char to the end of the line"})
; Move line to the end of the next line
; useful for move a comment above a line behind it
(nnoremap :zJ (preserve-paste :ddpkJ) {:silent true
                                       :desc "Move line to the end of the next line"})

(nnoremap :<Space> :za {:desc "Toggle fold"})
(vnoremap :<Space> :za {:desc "Toggle fold"})

; keeps freezing my vim??
; don't think I need this anymore
(inoremap :<F10> :<nop>)

(nmap :<ScrollWheelUp> :k {:desc "Scroll up"})
(nmap :<S-ScrollWheelUp> :k {:desc "Scroll up"})
(nmap :<ScrollWheelDown> :j {:desc "Scroll down"})
(nmap :<S-ScrollWheelDown> :j {:desc "Scroll down"})

; capitlize word under cursor
(nmap :gcw (preserve-cursor-loc "eb~" :q) {:silent true
                                           :desc "Capitalize word under cursor"})
; TODO: preserve highlight
(vmap :gcw (preserve-cursor-loc "<esc>eb~`qv" :q "v") {:silent true
                                                       :desc "Capitalize word in selection"})

(nnoremap :<leader>m ":messages<CR>" {:desc "show messages" :silent true})

; I have my delete key mapped to ctrl-backspace when held.
; see h key-notation
(inoremap :<C-H> :<C-w> {:desc "delete word before cursor"})
