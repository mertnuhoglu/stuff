
set -xv #on
wpprojects="wpci wpsb wpbp zwba"
for app in $wpprojects
do
 rapp="r${app:1}"
 
 ftpadmin="mertnuhoglu"
 
 ftpuser="$app"'user'
 
 cd ~/webapps/$rapp/
 touch .htaccess
 cat >.htaccess << 'EOF'
Options +FollowSymLinks
RewriteEngine on
RewriteCond %{HTTP_HOST} ^www\.(.*)\$ [NC]
RewriteRule ^(.*)\$ http://%1/\$1 [R=301,L]
EOF
 
 setfacl -m u:$ftpuser:x ~
 setfacl -R -m u:$ftpuser:rwx ~/webapps/$app/wp-content/
 setfacl -R -m d:u:$ftpuser:rwx ~/webapps/$app/wp-content/
 setfacl -R -m d:u:$ftpadmin:rwx ~/webapps/$app/wp-content/
 
 cp ~/files/wp/google162b89e2dcf7d30f.html ~/webapps/$app
done
