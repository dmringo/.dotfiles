export GOPATH="$HOME/go"

# at some point, this should maybe be set by an m4 macro during install export
MY_ZSH_HOME="$HOME/.dotfiles/zsh"

if [[ -e $MY_ZSH_HOME/secrets.zsh ]]; then source $MY_ZSH_HOME/secrets.zsh; fi;

export GOPATH=$HOME/.local/go

export PATH="\
$HOME/.local/bin:\
$HOME/perl5/bin:\
$GOPATH/bin:\
$HOME/.cabal/bin:\
$HOME/.rvm/bin:\
/usr/local/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin:\
/opt/X11/bin:"


# CPAN initialization suggested adding these (as well as the perl5 path in $PATH)
export PERL5LIB="/home/david/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL_LOCAL_LIB_ROOT="/home/david/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_MB_OPT="--install_base \"/home/david/perl5\""
export PERL_MM_OPT="INSTALL_BASE=/home/david/perl5"

export EDITOR="emacs"
export ESHELL="/usr/bin/zsh"
export CDPATH="${HOME}:${HOME}/proj"

alias l='ls -lh --color="auto" --group-directories-first'
alias la='ls -lhar --color="auto" --group-directories-first'

ostype=`uname -s`
case $ostype in
    Linux*)
        source $MY_ZSH_HOME/linuxenv.zsh
        ;;
    *)

        ;;
esac


# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
emc='emacsclient -c -a ""'
