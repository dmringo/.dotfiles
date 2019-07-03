# anything in specified in ~/.profile (for [ba]sh, e.g.) should be replicated in
# a zsh login shell (which only reads ~/.zprofile)

if [[ -n $MY_PATH ]]
then
  # see comments at end of .zshenv
  export PATH="$(printf %s "$MY_PATH:$PATH" | dedup_path)"
  unset MY_PATH
fi
