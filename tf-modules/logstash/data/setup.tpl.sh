wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list
apt-get update
apt-get install -y openjdk-8-jre
apt-get install -y logstash
LOCAL_IP=$$(ec2metadata --local-ipv4)
mv /etc/logstash/logstash.yml /etc/logstash/logstash.yml.bak
cat <<EOF > /etc/logstash/logstash.yml
http.host: "$${LOCAL_IP}"
path.data: /var/lib/logstash
path.config: /etc/logstash/conf.d
path.logs: /var/log/logstash
EOF
mkdir /etc/logstash/ssl
cat <<EOF > /etc/logstash/ssl/ca.crt
${ca_cert}
EOF
cat <<EOF > /etc/logstash/ssl/server.crt
${server_cert}
EOF
cat <<EOF > /etc/logstash/ssl/server.key
${server_key}
EOF
cat <<EOF > /etc/logstash/conf.d/logstash.conf
${config}
EOF
systemctl daemon-reload
systemctl enable logstash.service
systemctl start logstash.service
