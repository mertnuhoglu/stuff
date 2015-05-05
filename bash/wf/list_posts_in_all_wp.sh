#!/bin/bash
#Usage: sh ~/webapps/scripts/copy_wp.sh 
set -xv #on
cd ~/webapps
wpprojects="okuloncesi_blog roxalana_blog sy_blog dawp pdad pbys pdev pdst pifm pinf pjts pomc port pqzz pyeh whsd wozd wphp wpyr wtms wigu wpan wplu wpz2 wtpd wime wpar wpmh wpz3 wttt wisp wpbp wpmn wpzz wuzo wkvt wpci wpnp wsdn wvkr wmak wpdt wpos wshf ww4i wmbl wpeh wpsa wsmm zers wmrf wpek wpsb wspr zwba wbcc wmsw wpet wpsy wssh zwgg weoz wmtf wphd wpta wtdl wets wmyz wphe wpun wtm2 whor wn2i wphn wpym wtmn"
now=$(date +"%Y%m%d")
for wp in $wpprojects
do
    cd $wp
    echo $wp
    wp post list
    cd ..
 done

