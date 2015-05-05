#!/bin/bash
#Usage: sh copy_wp.sh whkr
set -xv #on

#### REPLACE
source=${1:-wpmn}
now=$(date +"%Y%m%d")
#### END REPLACE

sourcezip="$source"'_'"$now"'.zip'
sourcesql="$source"'_'"$now"'.sql'

# 0. backups
(cd ~/files/backup/ ; mkdir -p $source ;)
(cd ~/webapps/$source/ ; zip -qr ~/files/backup/$source/$sourcezip * ;)
(cd ~/webapps/$source/ ; wp db export ~/files/backup/$source/$sourcesql ;)

