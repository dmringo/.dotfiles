[[ -f "$HOME/.profile" ]] && emulate sh -c ". $HOME/.profile"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
fpath=( $ZDOTDIR $fpath )

# Maybe there are some secrets that should be loaded too
[[ -f "$ZDOTDIR/secrets.zsh" ]] && . "$ZDOTDIR/secrets.zsh"


if [[ -o login ]] && [[ $(uname -s) =~ Darwin ]]
then
  # We're on a mac and /etc/zprofile is likely going to mess with the PATH with
  # /usr/libexec/path_helper.  Since we do want the additional PATHs, it's not
  # really worth messing with that file. We *do* want to preserve anything that
  # we've already added though, in particular, the ordering (e.g. for preferring
  # gnu coreutils over the simpler but less featured BSD equivalents).  So, save
  # the PATH as MY_PATH, and check if that's set in .zprofile (sourced after the
  # system zprofile) to regain the appropriate ordering without losing anything
  # that path_helper might have added.
  MY_PATH="$PATH"
fi

# Source the local file last. This will be before zshrc, but that's ok.  It may
# define things useful for non-interactive use.  If it becomes an issue, maybe
# have a local.zsh and localrc.zsh or something?
[[ -f $ZDOTDIR/local.zsh ]] && . $ZDOTDIR/local.zsh
