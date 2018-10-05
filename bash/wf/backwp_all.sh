#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
wpprojects="okuloncesi_blog dawp wphn wphp mmgs okuloncesi_blog paep pbys pdad pdst pibp pifm pinf pomc port ppnd wpsa roxalana_blog sy_blog wpbp wmak wmtf wmyz wpci wpsb wpym wpz3 wsmm wttt zgg2 zwba"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
now=$(date +"%Y%m%d")
for wp in $wpprojects
do
 ./backwp_project.sh $wp
done
