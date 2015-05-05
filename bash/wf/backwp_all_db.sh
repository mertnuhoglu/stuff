#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
wpprojects="okuloncesi_blog roxalana_blog sy_blog wtdl wuzo wpmn wpsa wspr dawp wttt wphn pdad weoz wbph wsmm wime wphd wpym ww4i pbys pdev pdst pifm pinf pjts pomc port pqzz pyeh"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
now=$(date +"%Y%m%d")
for wp in $wpprojects
do
 wpsql="$wp"'_'"$now.sql"
 cd ~/files/backup/
 mkdir $wp
 cd ~/webapps/$wp/
 wp db export ~/files/backup/$wp/$wpsql
done
