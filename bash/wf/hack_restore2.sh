#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
wpprojects="wpsb wpci"
#wpprojects="wpsb zwba zwgg wpci"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
for app in $wpprojects
do
 appsql="$app"'_backup_20140720.sql'
 db='mertnuhoglu_'"$app"
 zipfile="$app"'_uploads_20140720.zip'
 cd ~/webapps/$app
 unzip -oq ~/files/backup/minimal/$app/$zipfile -d ~/webapps/$app/wp-content/uploads/
done
