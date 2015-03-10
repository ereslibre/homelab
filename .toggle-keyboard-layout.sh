#!/bin/bash
layout=$(setxkbmap -query | awk '/layout/{print $2}')
if [ $layout == 'us' ]
then
    setxkbmap es
else
    setxkbmap us
fi
killall -SIGUSR1 i3status
