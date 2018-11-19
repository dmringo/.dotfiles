
# if running bash (should be rare)
if [ -n "$BASH_VERSION" ]; then
  # Bash does *not* read ~/.bashrc by default as a login shell, but I'd like it
  # to, so include .bashrc if it exists.  It's also worth noting that if bash is
  # non-interactive, ~/.bash_profile and ~/.bash_login take priority (in that
  # order) over ~/.profile.  See
  # http://www.solipsys.co.uk/new/BashInitialisationFiles.html
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi

# Add a component to the PATH.  This really just exists so I can more
# conveniently document PATH additions item-wise.
function add2path() {
  PATH="$1:$PATH"
}

# Try to keep $HOME clean, keep Go-managed stuff out of sight
# TODO: Will this hurt if I start writing my own Go programs?
GOPATH="$HOME/.local/go"

# PATH components. -------------------------------------------------------------
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

# Don't need to keep this around
unset add2path

# Saint IGNUcius be praised
EDITOR="emacs"

# Helps emacs figure out what shell to use for `M-x shell`
ESHELL="/usr/bin/zsh"

# makes CDing to common directories easier
CDPATH="$HOME:$HOME/proj"

export PATH GOPATH EDITOR ESHELL CDPATH
