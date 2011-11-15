#!/bin/bash

# Load resources
 
xrdb -merge .Xresources
 
# Set up an icon tray
 
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 12 &

gnome-screensaver &

# network manager
if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

# battery manager
if [ -x /usr/bin/gnome-power-manager ] ; then
   sleep 3
   gnome-power-manager &
fi

#xmobar
xmobar ~/.xmobarrc &