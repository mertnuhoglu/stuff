#!/bin/sh
# quicklook from mucommander
# http://mu-j.com/mucommander/forums/viewtopic.php?f=2&t=728

QUICKLOOK='qlmanage'

if ps ax | grep -v grep | grep $QUICKLOOK > /dev/null
then
    killall $QUICKLOOK
else
    $QUICKLOOK -p "$1"
fi
