# zshrc - sourced by all interactive sessions

# Checking for dumb terminals - makes Tramp work when editing remote files
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return;

# pretty sure fpath has to be modified before compinit is called
if have_brew
then
  fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath)
fi

# cd to recent dirs
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# The following lines were added by compinstall
zstyle ':completion:*' completer \
       _expand _complete _ignored _match _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' format 'Completing %d ...'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list \
       'm:{[:lower:]}={[:upper:]}' \
       'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
       'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=0
zstyle ':completion:*' prompt '%e errors corrected for completion'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' word true

# This is what lets you navigate with e.g. up-line-or-history in completion
# menus.  Usually has to be triggered by entering a menu for ambiguous
# completion with an extra <tab>.
zstyle ':completion:*' menu selection

zstyle :compinstall filename '$ZDOTDIR/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}"/zsh/history
HISTSIZE=50000
SAVEHIST=50000
setopt appendhistory autocd extendedglob nomatch notify incappendhistory
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install


# make the directory for the history file if it doesn't exist
histdir="$(dirname $HISTFILE)"
[[ -d $histdir ]] || mkdir -p $histdir


# let zsh use completions defined for bash
autoload -U +X bashcompinit && bashcompinit

# Use bash-style word boundaries (i.e. anything not alphanumeric)
autoload -U select-word-style && select-word-style bash

# smarter functions for <M-n> and <M-p>
# These match all text entered in command line while searching
autoload -U up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

keybinds=(
  # <M-p>
  "^[p"   up-line-or-beginning-search
  # <M-n>
  "^[n"   down-line-or-beginning-search
  # Home key
  "^[[H"  beginning-of-line
  # End key
  "^[[F"  end-of-line
  # Delete key
  "^[[3~" delete-char-or-list
)

bindkey $keybinds

# Tell gpg about the TTY.  I think this only makes sense in an interactive
# session.  I also think this is necessary when GPG assumes no TTY, as is the
# case when $HOME/.gnupg/gpg.conf has 'no-tty' specified.
export GPG_TTY="$(tty)"

#--------- prompt setup

autoload -U promptinit && promptinit
# spaceship is 3rd party, all others are bundled with Zsh
my_prompts=(spaceship clint elite adam bart)
sys_prompts=($(prompt -l | sed -n 2p))

for p in $my_prompts
do
  # (r)$p in the subscript says "give me the value of the element that matches
  # the pattern $p"
  found_prompt=${sys_prompts[(r)$p]}
  if [[ -n $found_prompt ]]
  then
    # TODO: There's a better way to do this for sure
    case $found_prompt in
      spaceship )
        SPACESHIP_DIR_TRUNC_PREFIX="…/"
        SPACESHIP_DIR_TRUNC_REPO="false"
        SPACESHIP_CONDA_PREFIX="["
        SPACESHIP_CONDA_SUFFIX="]"
        SPACESHIP_CONDA_SYMBOL=""
        SPACESHIP_PROMPT_ORDER=(dir user host git conda exec_time
                                line_sep exit_code char)
        ;;
    esac

    prompt $p
    break
  fi
done


# Haskell stack
if cmd_exists stack
then
  # note that this requires the bashcompinit module loaded to work
  eval "$(stack --bash-completion-script stack)"
fi

# Alias ripgrep to use a config file
if cmd_exists rg
then
  alias rgi="rg -i"
  rg_conf="${XDG_CONFIG_HOME:-$HOME/.config}/ripgrep/config"
  if [[ -f "$rg_conf" ]]
  then
    # TODO: Don't do this. Just export the var and deal with it.
    # also move this. It's useful in all shells
    alias rg="RIPGREP_CONFIG_PATH=$rg_conf rg"
  fi
fi

# if we have direnv, get its hook setup
cmd_exists direnv && eval "$(direnv hook zsh)"

if cmd_exists docker
then
  alias dkr='docker'
  alias dkx='docker exec -i -t'
  alias dke='docker exec'
  alias dkps='docker ps'

  if cmd_exists docker-compose
  then
    alias dkc='docker-compose'
    alias dkcx='docker-compose exec'
    alias dkcu='docker-compose up -d'
    alias dkcU='docker-compose up'
    alias dkcd='docker-compose down'
  fi
fi

# Try to detect if we're using GNU coreutils so we know what extra options are
# valid
case "$(ls --version 2>/dev/null | head -n 1)" in
  *GNU*)
    extopts=" --color --group-directories-first"
    ;;
  *)
    extopts=""
    ;;
esac

alias l="ls -lh $extopts"
alias la="ls -lha $extopts"
alias ll="ls -lhA $extopts"

# If spack is around, we do a similar dance as we did for conda. This really
# needs to cooperate with at least one common setup where the system MODULEPATH
# is only set if it's not already set, and comes from the system zprofile,
# rather than zshenv.
_spack_base="$HOME/spack"
if [[ -d "${_spack_base}-$(uname -i)" ]]
then
  spack_base="${_spack_base}-$(uname -i)"
fi

_spack_src="${_spack_base}/share/spack/setup-env.sh"
[[ -f "$_spack_src" ]] && . "$_spack_src"

# ** less configuration

# i = ignore case unless any cap letters appear in search
# R = output raw ANSI color escape sequences for terminal to deal with
# M = long prompt (show line numbers and percentage)
LESS="iRM"
export LESS

# *** man wrapper with less termcap vars set for color.
#
# This relies on undocumented debugging variables that less(1) reads from the
# environment, but given how often they're used now, I suspect this capability
# won't ever be removed.
man() {
  # These variables could be set in the global environment too, but I'm not sure I
  # want that.

  # This follows the mappings used in the grml zsh setup
  # See https://wiki.archlinux.org/index.php/Color_output_in_console#less
  # and https://git.grml.org/?p=grml-etc-core.git
  # and https://unix.stackexchange.com/questions/119/colors-in-man-pages
  # and man 5 terminfo
  #
  # It's not clear exactly how many of these termcap codes will actually show up
  # in a manpage, but this covers a bunch of them.  Set LESS_TERMCAP_DEBUG=1 to
  # show the termcap codes instead of using the values below.
  #
  # setaf and setab use color codes as described in the terminfo manpage,
  # duplicated here for reference:
  #
  # Color     #define           Value    RGB
  # black     COLOR_BLACK       0        0, 0, 0
  # red       COLOR_RED         1        max,0,0
  # green     COLOR_GREEN       2        0,max,0
  # yellow    COLOR_YELLOW      3        max,max,0
  # blue      COLOR_BLUE        4        0,0,max
  # magenta   COLOR_MAGENTA     5        max,0,max
  # cyan      COLOR_CYAN        6        0,max,max
  # white     COLOR_WHITE       7        max,max,max

  # _mb: start bold          => bold, fg:red
  # _md: start blink         => bold, fg:cyan
  # _me: end   bold/blink    => clear all
  # _so: start reverse video => bold, bg:blue, fg:yellow
  # _se: end   reverse video => clear all
  # _us: start underline     => bold, fg:green
  # _ue: end   underline     => clear all
  # _mr: reverse             => reverse
  # _mh: dim                 => dim
  #
  # GROFF_NO_SGR is for Konsole and Gnome-terminal
  env LESS_TERMCAP_mb="$(tput bold; tput setaf 1)"               \
      LESS_TERMCAP_md="$(tput bold; tput setaf 6)"               \
      LESS_TERMCAP_me="$(tput sgr0)"                             \
      LESS_TERMCAP_so="$(tput bold; tput setaf 0; tput setab 6)" \
      LESS_TERMCAP_se="$(tput sgr0)"                             \
      LESS_TERMCAP_us="$(tput bold; tput setaf 2)"               \
      LESS_TERMCAP_ue="$(tput sgr0)"                             \
      LESS_TERMCAP_mr="$(tput rev)"                              \
      LESS_TERMCAP_mh="$(tput dim)"                              \
      GROFF_NO_SGR=1                                             \
      man "$@"
}

# Source a local rc file (for interactive use)
[[ -f $ZDOTDIR/localrc.zsh ]] && . $ZDOTDIR/localrc.zsh
