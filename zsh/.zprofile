# anything in specified in ~/.profile (for [ba]sh, e.g.) should be replicated in
# a zsh login shell (which only reads ~/.zprofile)

if [[ -n $MY_PATH ]]
then
  # see comments at end of .zshenv
  export PATH="$(printf %s "$MY_PATH:$PATH" | dedup_path)"
  unset MY_PATH
fi

# If spack is around, we do a similar dance as we did for conda. This really
# needs to be in .zprofile to cooperate with at least one common setup where the
# system MODULEPATH is only set if it's not already set, and comes from the
# system zprofile, rather than zshenv.
SPACK_SRC="$HOME/spack/share/spack/setup-env.sh"
[[ -f "$SPACK_SRC" ]] && . "$SPACK_SRC"
