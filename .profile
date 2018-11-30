# if running bash (should be rare)
if [ -n "$BASH_VERSION" ]; then
  # Bash does *not* read ~/.bashrc by default as a login shell, but I'd like it
  # to, so include .bashrc if it exists.  It's also worth noting that if bash is
  # non-interactive, ~/.bash_profile and ~/.bash_login take priority (in that
  # order) over ~/.profile.  See
  # http://www.solipsys.co.uk/new/BashInitialisationFiles.html
  if [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
  fi
fi

# Add a component to the PATH.  This really just exists so I can more
# conveniently document PATH additions item-wise.
add2path() {
  if [ -d "$1" ]; then PATH="$1:$PATH"; fi
}

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


if command -v brew 2>&1 > /dev/null
then
  BREW_PFX=$(brew config | grep HOMEBREW_PREFIX | cut -d' ' -f2)
  add2path "$BREW_PFX/bin"
  MANPATH="$BREW_PFX/share/man:$MANPATH"
  INFOPATH="$BREW_PFX/share/info:$INFOPATH"
fi

sys_type="$(uname -s | tr '[:upper:]' '[:lower:]')"

case "$sys_type" in
  darwin* )
    if [ -n "$BREW_PFX" ]
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
MANPATH="$(printf '%s' "$MANPATH" | dedup_path):"


# Don't need to keep this around
unset add2path

# Saint IGNUcius be praised
EDITOR="emacs"

# Helps emacs figure out what shell to use for `M-x shell`
ESHELL="/usr/bin/zsh"

# makes CDing to common directories easier
CDPATH="$HOME:$HOME/proj"

export PATH MANPATH INFOPATH GOPATH EDITOR ESHELL CDPATH
