#!/bin/bash
dest="$HOME/gdrive/keynote_resimler/screencapture"
f="${dest}/$(date +%Y%m%d%H%M%S).jpg"
screencapture -i $f
printf %s $f | pbcopy
