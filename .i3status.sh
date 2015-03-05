#!/bin/bash
i3status --config ~/.i3status.conf | while :
do
    read line
    layout=$(setxkbmap -query | awk '/layout/{print $2}')
    head="[{ \"full_text\": \"$layout\", \"color\": \"#FFFFFF\" },"
    echo "${line/[/$head}" || exit 1
done
