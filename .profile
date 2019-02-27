# OK idiot, this is for future you when you finally clean this mess up.
#
# You need to distinguish between environments necessary (or useful) for
# interactive and non-interactive use (e.g. PATH is useful to any process that
# calls exec(3), so it's important in both, but shell completion is really only
# useful interactively).
#
# Try to isolate the bits that you want portable between shells and have
# distinct scripts for the shell-specific stuff.  *Use*
# .bash{rc,_login,_profile} and .z{shrc,shenv,profile,login} and source the
# common stuff from there when appropriate.  Understand which of these files is
# read automatically and when.
#
# To that end, the following is useful:
#
#  - `sh`, when it's *really* an `sh` equivalent will read the file named by
#    the environment variable ENV.  Note that `zsh` does *not* do this when
#    invoked as `emulate sh`, but it *does* if `sh` is a symlink to `zsh`.  The
#    same is true of Bash (w.r.t. symlinks).  $HOME/.profile is only sourced on
#    startup.
#
#    > This variable can be used to set aliases and other items local to the
#    > invocation of a shell. The file referred to by ENV differs from
#    > $HOME/.profile in that .profile is typically executed at session start-up,
#    > whereas the ENV file is executed at the beginning of each shell
#    > invocation. The ENV value is interpreted in a manner similar to a dot
#    > script, in that the commands are executed in the current environment and
#    > the file needs to be readable, but not executable. However, unlike dot
#    > scripts, no PATH searching is performed. This is used as a guard against
#    > Trojan Horse security breaches.
#
#    See http://pubs.opengroup.org/onlinepubs/9699919799/xrat/V4_xcu_chap02.html
#
#  - Bash has some funny rules for which files are read and when.
#
#    See http://www.solipsys.co.uk/new/2BashInitialisationFiles.html
#
#  - Zsh has a (subjectively) saner set of rules
#
#    See http://zsh.sourceforge.net/Intro/intro_3.html
#
# Also worth perusing: https://docstore.mik.ua/orelly/unix/upt/index.htm

# keep all the XDG dirs under the same path
XDG_CONFIG_HOME="$HOME/.local/etc"
XDG_DATA_HOME="$HOME/.local/share"
XDG_CACHE_HOME="$HOME/.local/cache"
XDG_RUNTIME_DIR="$HOME/.local/run"


# do this in install.zsh?
mkdir -p "$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME" "$XDG_RUNTIME_DIR"
chmod 0700 "$XDG_RUNTIME_DIR"

ENV="$HOME/.config/sh/ENV"

# Add a component to the PATH.  This really just exists so I can more
# conveniently document PATH additions item-wise.
add2path() {
  if [ -d "$1" ]; then PATH="$1:$PATH"; fi
}


# Is homebrew being used?
if command -v brew 2>&1 > /dev/null
then
  have_brew() { return 0; }

  BREW_PFX=$(brew config | grep HOMEBREW_PREFIX | cut -d' ' -f2)
  add2path "$BREW_PFX/bin"
  MANPATH="$BREW_PFX/share/man:$MANPATH"
  INFOPATH="$BREW_PFX/share/info:$INFOPATH"
else
  have_brew() { return 1; }
fi

# if running bash (should be rare)
if [ -n "$BASH_VERSION" ]
then
  # Bash does *not* read ~/.bashrc by default as a login shell, but I'd like it
  # to, so include .bashrc if it exists.  It's also worth noting that if bash is
  # non-interactive, ~/.bash_profile and ~/.bash_login take priority (in that
  # order) over ~/.profile.  See
  # http://www.solipsys.co.uk/new/2BashInitialisationFiles.html
  if [ -f "$HOME/.bashrc" ]
  then
    . "$HOME/.bashrc"
  fi

  if have_brew
  then
    for comp in "$BREW_PFX"/etc/bash_completion.d/*
    do
      . "$comp"
    done
  fi
fi


# Try to keep $HOME cleanish, keep Go-managed stuff out of sight
GOPATH="$HOME/.local/go"

# PATH components. -------------------------------------------------------------

# This is sometimes missing, but I almost always find/put things here I want
add2path "/usr/local/bin"

# My local bin directory. *Most* of my local binaries will either reside here,
# or be symlinked here.
add2path "$HOME/.local/bin"
# Note: It's not really clear how much I should worry about symlinking
# built-from-source packages manually, in particular, those that may make
# assumptions about the (relative) locations of dependencies (e.g. python
# modules or shared libs).  Ideally, install scripts will manage all this
# properly, of course.


# Some systems can manage their own local binary directories.  Since the
# contents of theses tend to fluctuate between my computers, I don't symlink
# them (mostly because I'd forget to do so frequently enough to be a point of
# friction).  Usually, I'll prefer something here over something I put in
# ~/.local/bin.
add2path "$GOPATH/bin"  # GO managed bins
add2path "$HOME/.cabal/bin" # Cabal-managed bins


sys_type="$(uname -s | tr '[:upper:]' '[:lower:]')"

case "$sys_type" in
  darwin* )
    if have_brew
    then
      # Homebrew paths for GNU coreutils stuff
      gnubin="$BREW_PFX/opt/coreutils/libexec/gnubin"
      gnuman="$BREW_PFX/opt/coreutils/libexec/gnuman"
      if [ -d "$gnubin" ] && [ -d "$gnuman" ]
      then
        add2path "$gnubin"
        MANPATH="$gnuman:$MANPATH"
      fi

    fi

    clip() {
      case "$1" in
        ""|i|in )
          cmd=pbcopy
          ;;
        o|out )
          cmd=pbpaste
          ;;
        * )
          printf "Unrecognized option: %s\n" "$1"
          return 1
          ;;
      esac
      if [ -n "$2" ]
      then
        "$cmd" < "$2"
      else
        "$cmd"
      fi
    }
    ;;

  linux* )
    alias o='xdg-open'

    clip() {
      case "$1" in
        f|filter|i|in|o|out )
          mode="$1"
          shift
          ;;
        "" )
          mode=i
          ;;
        * )
          printf "Unrecognized option: %s\n" "$1"
          return 1
          ;;
      esac
      xclip -"$mode" -selection clipboard "$@"
    }
    ;;
  * )
    ;;
esac

[ -n "$BREW_PFX" ]
# I almost always use Conda for managing python-y stuff, but only the minimal
# distribution.  The full distribution comes with a bunch of packages that I
# prefer to maintain via other channels (e.g. pandoc)
[ -d "$HOME/miniconda3/bin" ] && add2path "$HOME/miniconda3/bin"


# It's possible that some of these components were already in the PATH, so
# remove the duplicates (script in the bin/ directory of the dotfiles)
PATH="$(printf '%s' "$PATH" | dedup_path)"
INFOPATH="$(printf '%s' "$INFOPATH" | dedup_path):"

# The terminating colon is important here! Basically, it has the effect of
# appending the system man path(s) at the end of this list.  See manpath(1) for
# details or https://askubuntu.com/q/197461
if [ -n "$MANPATH" ]
then
  MANPATH="$(printf '%s' "$MANPATH" | dedup_path):"
fi


# Don't need to keep this around
unset add2path

# Saint IGNUcius be praised
EDITOR="emacs"

# Helps emacs figure out what shell to use for `M-x shell`
ESHELL="/usr/bin/zsh"

# RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep"
# DOCKER_CONFIG="$HOME/.config/docker/config"

for var in PATH MANPATH INFOPATH GOPATH EDITOR ESHELL CDPATH
do
  if [ -n "$var" ]
  then
    export $var
  fi
done
