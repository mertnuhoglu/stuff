#!/bin/bash
#Usage: sh ~/webapps/scripts/update_wp.sh 
set -xv #on
wpprojects="amerika_blog dawp etwp hpwp mmgs mun2 mun3 munf okuloncesi_blog paep pbys pdad pdst pibp pifm pinf pjts pnpn pokr pomc port ppnd pqzz pyeh roxalana_blog saw2 saw3 sawp senindemokrasin_blog sy_blog tbph tcii tmn4 tmn5 ttm3 tvwp unf2 wbcc weoz wets whor whsd wigu wime wisp wkvt wn2i wozd wpan wpar wpba wpbp wpci wpdt wpeh wpek wpet wphd wphe wphn wphp wplu wpmh wpmn wpnp wpos wpsa wpsb wpsy wpta wpun wpym wpyr wpz2 wpz3 wpzz wsdn wshf wsmm wspr wssh wtdl wtm2 wtmn wtms wttt wuzo wvkr ww4i zers zwba zwgg"
#wpprojects="okuloncesi_blog roxalana_blog sy_blog wtdl wuzo wpmn wpsa wspr dawp wttt wphn pdad weoz wbph wsmm wphd wpym ww4i pbys pdev pdst pifm pinf pjts pomc port pqzz pyeh wime"
#wpprojects="wpci wpsb wpbp wdpr zwba wpbk"
for wp in $wpprojects
do
 cd ~/webapps/$wp/
 wp core download --force
 wp plugin update --all
 #wp core update
done

