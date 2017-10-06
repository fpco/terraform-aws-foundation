apt-get update
apt-get install -y nginx
BASIC_AUTH_USERNAME="$(${credstash_get_cmd} ${nginx_username_key} ${credstash_context})"
BASIC_AUTH_PASSWORD="$(${credstash_get_cmd} ${nginx_password_key} ${credstash_context})"
echo -n "$BASIC_AUTH_USERNAME:" > /etc/nginx/.htpasswd
openssl passwd -apr1 "$BASIC_AUTH_PASSWORD" >> /etc/nginx/.htpasswd
LOCAL_DNS=$(ec2metadata --local-hostname)

cat <<EOF > /etc/nginx/sites-available/default
server {
    listen 9201;
    server_name $LOCAL_DNS;
    access_log /var/log/nginx/access.log;
    error_log /var/log/nginx/error.log info;
    location / {
        proxy_pass http://localhost:9200;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
        auth_basic "Restricted Space";
        auth_basic_user_file /etc/nginx/.htpasswd;
    }
}
EOF
service nginx reload


cat <<EOF > /etc/filebeat/prospectors/nginx.yaml
filebeat.prospectors:
- input_type: log
  paths:
    - '/var/log/nginx/access.log'
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: aws
      source: elasticsearch-proxy
      formats:
        - nginx_access
      transport: filebeat
- input_type: log
  paths:
    - '/var/log/nginx/error.log'
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: aws
      source: elasticsearch-proxy
      formats:
        - nginx_error
      transport: filebeat
EOF
