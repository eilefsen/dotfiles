#!/bin/sh

if [ -e $XDG_CONFIG_HOME/wmenu.run ]
then
	eval $(cat $XDG_CONFIG_HOME/wmenu.run | wmenu)
else
	eval $(cat /etc/wmenu.run | wmenu)
fi
