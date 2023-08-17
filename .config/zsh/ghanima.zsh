# heavily inspired by https://github.com/spaceship-prompt/spaceship-prompt/
GHANIMA_PROMPT_ORDER=(
  status
  vi-mode
  dir
  nodejs
  newline
  context
)

GHANIMA_RPROMPT_ORDER=(
  git
  nix
)

GHANIMA_ROOT="$HOME/.config/zsh"
GHANIMA_LIBS=(
  "lib/utils.zsh"
  "lib/cache.zsh"
  "lib/cli.zsh"
  "lib/emoji.zsh"
  "lib/sections.zsh"
  "lib/prompts.zsh"
  "lib/hooks.zsh"
  "lib/core.zsh"
)

for lib in "${GHANIMA_LIBS[@]}"; do
  builtin source "$GHANIMA_ROOT/$lib"
  ghanima::precompile "$GHANIMA_ROOT/$lib"
done

ghanima::precompile "$GHANIMA_ROOT/$0"


prompt_ghanima_setup() {
  autoload -Uz add-zsh-hook

  # This variable is a magic variable used when loading themes with zsh's prompt
  # function. It will ensure the proper prompt options are set.
  prompt_opts=(cr subst percent sp)

  # Borrowed from promptinit, sets the prompt options in case the prompt was not
  # initialized via promptinit.
  setopt noprompt{bang,cr,percent,subst} "prompt${^prompt_opts[@]}"

  # Initialize builtin functions
  zmodload zsh/datetime
  zmodload zsh/mathfunc

  add-zsh-hook precmd ghanima::hooks::precmd
  # add-zsh-hook preexec ghanima::hooks::preexec

  ghanima::core::load
}

prompt_ghanima_setup "$@"
