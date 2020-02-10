#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
userxprofile=$HOME/.xprofile
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]
then
  xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]
then
  xmodmap $sysmodmap
fi

if [ -f "$userresources" ]
then
  xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]
then
  xmodmap "$usermodmap"
fi

if [ -f "$userxprofile" ]
then
  . "$userxprofile"
fi



# start some nice programs

dunst &
[ -x ~/.fehbg ] && ~/.fehbg &

i3log="~/.local/share/i3/i3-$(date +%F%T).log"

exec i3 

