#!/bin/bash
set -x

apt-get update
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list

# Install dependencies
${credstash_install_snippet}
apt-get install -y openjdk-8-jre
apt-get install -y logstash=1:${logstash_version}-1

# Make sure Logstash can read local logs.
usermod -a -G adm logstash

/usr/share/logstash/bin/logstash-plugin update logstash-filter-grok

# Logstash settings
LOCAL_IP=$(ec2metadata --local-ipv4)
mv /etc/logstash/logstash.yml /etc/logstash/logstash.yml.bak
cat <<EOF > /etc/logstash/logstash.yml
http.host: "$LOCAL_IP"
path.data: /var/lib/logstash
path.config: /etc/logstash/conf.d
path.logs: /var/log/logstash
config.reload.automatic: true
config.reload.interval: 60
${extra_settings}
EOF

mkdir /etc/logstash/patterns
cat <<EOF > /etc/logstash/patterns/extra-grok-patterns
${extra_grok_patterns}
EOF

mkdir /etc/logstash/ssl

${credstash_get_cmd} -n ${credstash_ca_cert_name} ${credstash_context} > /etc/logstash/ssl/ca.crt
${credstash_get_cmd} -n ${credstash_server_cert_name} ${credstash_context} > /etc/logstash/ssl/server.crt
${credstash_get_cmd} -n ${credstash_server_key_name} ${credstash_context} > /etc/logstash/ssl/server.key

# Essential and minimal configuration for logstash (Beats and HTTP inputs and Elasticsearch output)
cat <<EOF > /etc/logstash/conf.d/00-logstash.conf
${config}
EOF

cat <<EOF > /etc/logstash/conf.d/10-logstash_extra.conf
${extra_config}
EOF

# Create a cron job for pulling dynamic config
cat <<EOF > /etc/logstash/credstash-cronjob.sh
#!/bin/bash
${credstash_get_cmd} -n ${credstash_dynamic_config_name} ${credstash_context} 2>/dev/null >/etc/logstash/conf.d/30-logstash-dynamic.conf
EOF
chmod a+x /etc/logstash/credstash-cronjob.sh
TMP_CRON=$(mktemp -t "dyn-config-cron-job-XXXXXX.txt")
crontab -l > $TMP_CRON
echo "* * * * * /etc/logstash/credstash-cronjob.sh" >> $TMP_CRON
crontab $TMP_CRON
rm $TMP_CRON

systemctl daemon-reload
systemctl enable logstash.service
systemctl start logstash.service



## Setup Metricbeat
## ----------------

apt-get install -y metricbeat

cat <<EOF > /etc/metricbeat/metricbeat.yml
metricbeat.modules:
- module: system
  metricsets:
    - cpu
    - load
    - diskio
    - fsstat
    - memory
    - process
    - network
  interfaces: [eth0]
  enabled: true
  period: 30s
  processes:
    - 'java'
    - 'kibana'
  cpu_ticks: false
  fields_under_root: true
  fields:
    index_prefix: metricbeat-elk
    metric_info:
      origin: elk-logstash${name_suffix}
      source: metricbeat
output.logstash:
  hosts: ["localhost:5045"]
EOF

systemctl daemon-reload
systemctl enable metricbeat.service
systemctl start metricbeat.service


${extra_setup_snippet}
