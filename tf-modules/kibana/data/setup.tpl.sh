wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list
apt-get update
apt-get install -y kibana
LOCAL_IP=$$(ec2metadata --local-ipv4)
mv /etc/kibana/kibana.yml /etc/kibana/kibana.yml.bak
cat <<EOF > /etc/kibana/kibana.yml
elasticsearch.url: "${elasticsearch_url}"
server.host: "$${LOCAL_IP}"
EOF
systemctl daemon-reload
systemctl enable kibana.service
systemctl start kibana.service
