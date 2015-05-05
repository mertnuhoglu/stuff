#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
cd ~/webapps
wpprojects="wmyz wmtf wmak wmsw wmrf wmbl"
now=$(date +"%Y%m%d")
for wp in $wpprojects
do
    cd $wp
    echo $wp
	 wp plugin deactivate digg-digg
	 wp plugin uninstall digg-digg
    cd ..
 done


