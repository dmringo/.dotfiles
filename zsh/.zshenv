[[ -f "$HOME/.profile" ]] && emulate sh -c ". $HOME/.profile"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
fpath=( $ZDOTDIR $fpath )

# Maybe there are some secrets that should be loaded too
[[ -f "$ZDOTDIR/secrets.zsh" ]] && . "$ZDOTDIR/secrets.zsh"

# I almost always use Conda for managing python-y projects, but only the minimal
# distribution. Recent versions encourage you to source their init script and
# activate the base environment, but I only want `conda` available by default. I
# can always activate the base environment as necessary.
CONDA_SRC="$HOME/miniconda3/etc/profile.d/conda.sh"
if [ -f "$CONDA_SRC" ]
then
  . "$CONDA_SRC"
  # WORKON_HOME is used by virtualenvwrapper.sh and pyvenv for Emacs
  WORKON_HOME="$(conda config --show envs_dirs | awk 'NR==2{print$2}')"
  export WORKON_HOME

fi

# If spack is around, we do a similar dance as we did for conda
SPACK_SRC="$HOME/spack/share/spack/setup-env.sh"
[[ -f "$SPACK_SRC" ]] && . "$SPACK_SRC"



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
