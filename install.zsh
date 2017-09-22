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

# Unless DOT_HOME is set, use absolute path of this script
DOT_HOME=${DOT_HOME:-${0:A:h}}

# keep track of the return status of linking operations
lnStat=0


_log(){
    print $@
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
                let "lnStat |= 1"
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


    if [[ -e $trg && `stat -Lf '%i' $src` = `stat -Lf '%i' $trg` ]]
    then

    else
        _log -f "linking %s --> %s\n" $src $trg
        ln -si $src $trg
        let "lnStat |= $?"
    fi
}


_log "using $DOT_HOME as base location of '.dotfiles' repo"

_ln $DOTHOME/zsh/.zshrc $HOME/.zshrc
_ln $DOTHOME/zsh/.zshenv $HOME/.zshrc
_ln $DOTHOME/emacs $HOME/.emacs.d
_ln $DOTHOME/emacs/init.el $HOME/.emacs # convenient, but not necessary

if [[ $lnStat -ne 0 ]]
then
    _log "Oh no! Some linking failed!"
    return 1
else
    _log "Install completed successfully.  Probably."
fi
