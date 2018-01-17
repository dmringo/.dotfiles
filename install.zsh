#!/usr/bin/env zsh

# This assumes that zsh is installed on the system.  At some point, I should
# write a boot script using system-specific package managers to install the
# basics (zsh, git, perl).
#
# This script should eventually be able to install all the things I use
# frequently (stable emacs, stack, ripgrep, chrome). For now it just makes sure
# my dot files are appropriately linked.  Probably.

# fail hard, plz
set -e
# set -x

# Unless DOT_HOME is set, use absolute path of this script
DOT_HOME=${DOT_HOME:-${0:A:h}}

# keep track of the return status of linking operations
lnStat=0


# Eventually will make this a real log-ish function, probably with
# verbosity level either as a parameter or set in some global variable.
_log(){
    print $@
}

# Check if directory path exists.  Create it if it doesn't
maybe_mkdir(){
    if [[ ! -d $1 ]]; then mkdir -p $1; fi;
}

# Convenience linking function.  It expects exactly two arguments (source and
# target for linking) which should be fully "specified".  That is, relative
# paths are fine, but specifying the target as an existing destination directory
# alone is not (unlike `ln`).
_ln(){

    src=$DOT_HOME/$1
    trg=$2

    if [[ -e $trg ]]
    then
        read -r -k 1 'ans?'"$trg already exists. (r)eplace/(s)kip/(b)ackup?"
        case $ans in
            [rR] )
                rm -rf $trg ;;
            [sS] )
                # skipping means we didn't link something
                lnStat=1
                return ;;
            [bB] )
                bkp=$trg.`date +''%F!%T`.bak
                _log -f "backing up %s as %s\n" $trg $bkp
                mv $trg $bkp
                ;;
            *    )
                _log -f "unrecognized response: %s\n" $ans
                return 1
        esac
    fi

    # create path if necessary
    maybe_mkdir `dirname $trg`
    _log -f "linking %s --> %s\n" $src $trg
    ln -s $src $trg
    lnStat=$((lnStat | ?))
    return 0
}


_log "using $DOT_HOME as base location of '.dotfiles' repo"

_ln $DOT_HOME/zsh/.zshrc         $HOME/.zshrc
_ln $DOT_HOME/zsh/.zshenv        $HOME/.zshenv
_ln $DOT_HOME/emacs              $HOME/.emacs.d
_ln $DOT_HOME/emacs/init.el      $HOME/.emacs # convenient, but not necessary
_ln $DOT_HOME/ssh/config         $HOME/.ssh/config

# TODO: Need to generate these per system somehow.  Good case for switching to
# xmonad
# _ln $DOT_HOME/i3/config          $HOME/.config/i3/config
# _ln $DOT_HOME/i3/i3status-config $HOME/.config/i3status/config

for f in $DOT_HOME/bin/*
do
    _ln $f $HOME/.local/bin/$(basename $f)
done



if [[ $lnStat -ne 0 ]]
then
    _log "Oh no! Some linking failed! Better inspect things."
    return 1
else
    _log "Install completed successfully.  Probably."
fi
