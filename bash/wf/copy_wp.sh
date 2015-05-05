#!/bin/bash
#Usage: sh copy_wp.sh whkr
set -xv #on

#### REPLACE
source=wpbp
dest=tbph
url=test.burakpehlivan.org
date=20140724
#### END REPLACE

sourcezip="$source"'_wp_content_'"$date"'.zip'
destzip="$dest"'_wp_content_'"$date"'.zip'
destsql="$dest"'_backup_'"$date"'.sql'
sourcesql="$source"'_backup_'"$date"'.sql'

# 0. backups
(cd ~/files/backup/ ; mkdir $dest ;)
(cd ~/files/backup/ ; mkdir $source ;)
(cd ~/webapps/$source/wp-content ; zip -qr ~/files/backup/$source/$sourcezip * ;)
(cd ~/webapps/$dest/wp-content ; zip -qr ~/files/backup/$dest/$destzip * ;)
(cd ~/webapps/$dest/ ; wp db export ~/files/backup/$dest/$destsql ; )

# 1. dosyaları kopyala
cd ~/webapps/$dest/
rm -fr wp-content/*
unzip -oq ~/files/backup/$source/$sourcezip -d wp-content/

# 2. veritabanını aktar
(cd ~/webapps/$source/ ; wp db export ~/files/backup/$source/$sourcesql ;)
(cd ~/webapps/$dest/ ; wp db import ~/files/backup/$source/$sourcesql ;)

# 3. home_url düzelt
wp option update siteurl "http://$url"
wp option update home "http://$url"
