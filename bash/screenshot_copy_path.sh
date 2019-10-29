#!/bin/bash
f="$HOME/Pictures/screenshots/$(date +%Y%m%d%H%M%S).png"
screencapture -i $f
printf %s $f | pbcopy
