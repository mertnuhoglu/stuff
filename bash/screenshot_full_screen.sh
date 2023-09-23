#!/bin/bash
dest="$HOME/gdrive/keynote_resimler/screencapture"
f="${dest}/scs$(date +%Y%m%d)_$(date +%H%M%S).jpg"
screencapture -x -t jpg -D 2 $f
printf %s $f | pbcopy
