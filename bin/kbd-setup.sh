#!/bin/sh
#
# This script should set up keyboard-related configurations. It's largely taken
# from https://superuser.com/a/350336
#
# To use this with a udev rule, matching ATTRS{interface}=="Keyboard" may
# keyboards specifically, but it's good to test what ATTRS actually show up with
# udevadm. This script should be run in the background so udev can finish
# setting things up.
#
# Sample rule:
# ACTION=="add", SUBSYSTEM=="input", ATTRS{interface}=="Keyboard", SUBSYSTEMS=="usb", RUN+="/path/to/wrapper-script", OWNER="username"

sleep 1
DISPLAY=${DISPLAY:-:0}
HOME=${HOME:-/home/dringo/}
XAUTHORITY=${XAUTHORITY:-$HOME/.Xauthority}
export DISPLAY XAUTHORITY HOME

printf "%s kbd\\n" "$(date +'%x %X %N')" >> $HOME/udevtest.log

xset r rate 200 100
setxkbmap -option ctrl:nocaps
