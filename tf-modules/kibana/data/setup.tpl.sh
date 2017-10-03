wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list
apt-get update
apt-get install -y kibana=${kibana_version}
LOCAL_IP=$(ec2metadata --local-ipv4)
mv /etc/kibana/kibana.yml /etc/kibana/kibana.yml.bak
cat <<EOF > /etc/kibana/kibana.yml
elasticsearch.url: "${elasticsearch_url}"
server.host: "localhost"
EOF
systemctl daemon-reload
systemctl enable kibana.service
systemctl start kibana.service

apt-get install -y nginx

# Basic auth
BASIC_AUTH_USERNAME="$(${credstash_get_cmd} ${nginx_username_key} ${credstash_context})"
BASIC_AUTH_PASSWORD="$(${credstash_get_cmd} ${nginx_password_key} ${credstash_context})"
echo -n "$BASIC_AUTH_USERNAME:" > /etc/nginx/.htpasswd
openssl passwd -apr1 "$BASIC_AUTH_PASSWORD" >> /etc/nginx/.htpasswd

mkdir /var/www/html/kibana-status
echo "Status: Initializing" > /var/www/html/kibana-status/index.html
LOCAL_DNS=$(ec2metadata --local-hostname)
cat <<EOF > /etc/nginx/sites-available/kibana
server {
    listen 80 default_server;
    listen [::]:80 default_server;
    server_name _;
    return 301 https://\$$host\$$request_uri;
}
server {
    listen 5602;
    server_name $LOCAL_DNS;
    access_log /var/log/nginx/access.log;
    error_log /var/log/nginx/error.log info;
    location / {
        proxy_pass http://localhost:5601;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        auth_basic "Restricted Space";
        auth_basic_user_file /etc/nginx/.htpasswd;
    }
}
server {
    listen 5603;
    root /var/www/html/kibana-status;
    location / {
    }
}
EOF

rm /etc/nginx/sites-enabled/default
ln -s /etc/nginx/sites-available/kibana /etc/nginx/sites-enabled/kibana
service nginx configtest
service nginx reload


# Create a cron job for checking the status of Kibana and nginx for ELB Health Checks
cat <<EOF > /etc/kibana/status-cronjob.sh
#!/bin/bash
KIBANA_RESPONSE_CODE=\$(curl --write-out %{http_code} --silent --output /dev/null localhost:5601)
NGINX_RESPONSE_CODE=\$(curl --write-out %{http_code} --silent --output /dev/null $LOCAL_DNS:5602)
if [ "\$KIBANA_RESPONSE_CODE" == 200 ] && [ "\$NGINX_RESPONSE_CODE" == 401 ]; then
  echo "Status: Good" > /var/www/html/kibana-status/index.html
else
  rm /var/www/html/kibana-status/index.html
fi
EOF
chmod a+x /etc/kibana/status-cronjob.sh
TMP_CRON=$(mktemp -t "kibana-status-XXXXXX.txt")
crontab -l > $TMP_CRON
echo "* * * * * /etc/kibana/status-cronjob.sh" >> $TMP_CRON
crontab $TMP_CRON
rm $TMP_CRON
