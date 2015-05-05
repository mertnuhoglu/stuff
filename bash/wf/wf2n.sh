#!/bin/bash
#Usage: sh wf2n.sh whkr
set -xv #echo on

app=$1
rapp="r${app:1}"

#### REPLACE
description='HayatDefteri Proje Bilgi Yönetim Sistemi'
blogname='HayatDefteri BYS'
demo=false
p2=true
#### END REPLACE

ftpadmin="mert"

mailbox="$app"'_inpost'

ftpuser="$app"'user'


(cd ~/files/backup/;mkdir $app;mkdir $app/job1;mkdir $app/job2)
cd ~/webapps/$app/
wp plugin delete hello
wp user update 1 --user_pass=m0e1r1t0

wp user create mertnuhoglu mert.nuhoglu@gmail.com --role=administrator --user_pass=m0e1r1t0
wp user create 101kaplumbaga 101kaplumbaga@gmail.com --role=contributor --user_pass=sincap101

wp option update admin_email 'mert.nuhoglu+admin@gmail.com'
wp option update blogdescription "$description"
wp option update blogname "$blogname"
wp option update date_format 'Y/m/d'
wp option update gmt_offset +2

wp option update mailserver_url 'mail.webfaction.com'
wp option update mailserver_login "$mailbox" 
wp option update mailserver_pass "selami00" 
wp option update enable_app 1 
wp option update enable_xmlrpc 1

wp option update comment_moderation 0
wp option update comments_notify 1
wp option update moderation_notify 1
wp option update permalink_structure '/%post_id%/%postname%/'
wp option update category_base 'konu'
wp option update tag_base 'indeks'
wp option update posts_per_page 20

if $p2 ; then
    wp theme install p2
    wp theme activate p2

    wp option update blog_public 0

    unzip -oq ~/files/wp/email-mention-notification.zip -d ~/webapps/$app/wp-content/plugins/
    wp plugin activate email-mention-notification

    wp plugin activate configure-smtp
    unzip -oq ~/files/wp/gravityforms.zip -d ~/webapps/$app/wp-content/plugins/
    wp plugin activate gravityforms
    wp plugin install easy-table --activate
    wp plugin install follow-button-for-jetpack --activate
    wp plugin install jetpack --activate
    wp plugin install limit-login-attempts --activate
    wp plugin install markdown-for-p2 --activate
    wp plugin install private-only --activate
    wp plugin install wordpress-wiki-plugin --activate
    wp plugin install backwpup --activate
    wp plugin install mention-me --activate
    wp plugin install p2-resolved-posts --activate
    wp plugin activate configure-smtp

    exit 0
fi

wp plugin activate akismet
wp plugin activate configure-smtp
unzip -oq ~/files/wp/gravityforms.zip -d ~/webapps/$app/wp-content/plugins/
wp plugin activate gravityforms
wp plugin install subscribe-to-comments --activate
wp plugin install rss-footer --activate
wp plugin install limit-login-attempts --activate
wp plugin install in-series --activate
wp plugin install thank-me-later --activate
wp plugin install jetpack --activate
wp plugin install backwpup --activate
wp plugin install digg-digg --activate
wp plugin install xml-sitemap-feed --activate
wp plugin install google-analytics-for-wordpress --activate
wp plugin install follow-button-for-jetpack --activate
wp plugin install easy-table --activate
wp plugin install wordpress-importer --activate

if $demo ; then
    exit 0
fi

wp option update blog_public 1

cd ~/webapps/$rapp/
touch .htaccess
cat >.htaccess << 'EOF'
Options +FollowSymLinks
RewriteEngine on
RewriteCond %{HTTP_HOST} ^www\.(.*)\$ [NC]
RewriteRule ^(.*)\$ http://%1/\$1 [R=301,L]"
EOF

setfacl -m u:$ftpuser:x ~
setfacl -R -m u:$ftpuser:rwx ~/webapps/$app/wp-content/
setfacl -R -m d:u:$ftpuser:rwx ~/webapps/$app/wp-content/
setfacl -R -m d:u:$ftpadmin:rwx ~/webapps/$app/wp-content/

cp ~/files/wp/google162b89e2dcf7d30f.html ~/webapps/$app
