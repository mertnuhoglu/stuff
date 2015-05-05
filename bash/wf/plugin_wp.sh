#!/bin/bash
#Usage: sh ~/webapps/scripts/update_wp.sh 
set -xv #on
wpprojects="okuloncesi_blog roxalana_blog sy_blog wtdl wuzo wpmn wpsa wspr dawp wttt"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
for wp in $wpprojects
do
 cd ~/webapps/$wp/
 wp plugin deactivate wp-greet-box
 wp plugin uninstall wp-greet-box
done

