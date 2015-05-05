#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
wpprojects="oaym"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
now=$(date +"%Y%m%d")
for wp in $wpprojects
do
 (cd ~/webapps/$wp/ ; zip -qr ~/files/backup/$wp/$wp_$now.zip * )
 cd ~/files/backup/$wp
done
