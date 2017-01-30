set -x

wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list

# Install dependencies
${credstash_install_snippet}
apt-get install -y openjdk-8-jre
apt-get install -y logstash

# Logstash settings
LOCAL_IP=$$(ec2metadata --local-ipv4)
mv /etc/logstash/logstash.yml /etc/logstash/logstash.yml.bak
cat <<EOF > /etc/logstash/logstash.yml
http.host: "$${LOCAL_IP}"
path.data: /var/lib/logstash
path.config: /etc/logstash/conf.d
path.logs: /var/log/logstash
config.reload.automatic: true
config.reload.interval: 30
${extra_settings}
EOF

mkdir /etc/logstash/ssl

cat <<EOF > /etc/logstash/ssl/ca.crt
${ca_cert}
EOF

cat <<EOF > /etc/logstash/ssl/server.crt
${server_cert}
EOF
echo "Running: ${credstash_get_cmd} -n ${credstash_server_key_name}"
${credstash_get_cmd} -n ${credstash_server_key_name} > /etc/logstash/ssl/server.key

cat <<EOF > /etc/logstash/conf.d/logstash.conf
${config}
EOF

# Create a cron job for pulling dynamic config
cat <<EOF > /etc/logstash/credstash-cronjob.sh
#!/bin/bash
${credstash_get_cmd} -n ${credstash_dynamic_config_name} 2>/dev/null >/etc/logstash/conf.d/logstash-dynamic.conf
EOF
chmod a+x /etc/logstash/credstash-cronjob.sh
TMP_CRON=$$(mktemp "/tmp/dyn-config-cron-job-XXXXXX.txt")
crontab -l > $$TMP_CRON
echo "${credstash_dynamic_config_cron} /etc/logstash/credstash-cronjob.sh" >> $$TMP_CRON
crontab $$TMP_CRON
rm $$TMP_CRON

systemctl daemon-reload
systemctl enable logstash.service
systemctl start logstash.service

${extra_setup_snippet}
