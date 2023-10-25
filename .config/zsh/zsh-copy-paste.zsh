local copy_command='xclip -in -selection clipboard'
local paste_command='xclip -out -selection clipboard'

[[ $(uname) == 'Darwin' ]] && copy_command='pbcopy'
[[ $(uname) == 'Darwin' ]] && paste_command='pbpaste'

my_zvm_vi_yank() {
  zvm_vi_yank
  eval "echo -en \"${CUTBUFFER}\" | $copy_command"
}

my_zvm_vi_delete() {
  zvm_vi_delete
  eval "echo -en "${CUTBUFFER}" | $copy_command"
}

my_zvm_vi_change() {
  zvm_vi_change
  eval "echo -en "${CUTBUFFER}" | $copy_command"
}

my_zvm_vi_change_eol() {
  zvm_vi_change_eol
  eval "echo -en "${CUTBUFFER}" | $copy_command"
}

my_zvm_vi_put_after() {
  CUTBUFFER=$(xclip -out -selection clipboard)
  zvm_vi_put_after
  zvm_highlight clear # zvm_vi_put_after introduces weird highlighting for me
}

my_zvm_vi_put_before() {
  CUTBUFFER=$(xclip -out -selection clipboard)
  zvm_vi_put_before
  zvm_highlight clear # zvm_vi_put_before introduces weird highlighting for me
}

zvm_after_lazy_keybindings() {
  zvm_define_widget my_zvm_vi_yank
  zvm_define_widget my_zvm_vi_delete
  zvm_define_widget my_zvm_vi_change
  zvm_define_widget my_zvm_vi_change_eol
  zvm_define_widget my_zvm_vi_put_after
  zvm_define_widget my_zvm_vi_put_before

  zvm_bindkey visual 'y' my_zvm_vi_yank
  zvm_bindkey visual 'd' my_zvm_vi_delete
  zvm_bindkey visual 'x' my_zvm_vi_delete
  zvm_bindkey vicmd  'C' my_zvm_vi_change_eol
  zvm_bindkey visual 'c' my_zvm_vi_change
  zvm_bindkey vicmd  'p' my_zvm_vi_put_after
  zvm_bindkey vicmd  'P' my_zvm_vi_put_before
}
