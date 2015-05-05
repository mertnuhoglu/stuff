#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
wpprojects="wpsb"
# wpprojects="wpsb zwba zwgg"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
for app in $wpprojects
do
 appsql="$app"'_backup_20140720.sql'
 db='mertnuhoglu_'"$app"
 cd ~/webapps/$app
 mysqldump $db  -u $db -pTrabzonYolu wp_options > wp_options.sql
 wp db import ~/files/backup/minimal/$app/$appsql
 wp db import wp_options.sql
done
