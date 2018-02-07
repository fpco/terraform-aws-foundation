#!/bin/bash

hostname ${node_name}
echo ${node_name} > /etc/hostname
echo 127.0.1.1 ${node_name} >> /etc/hosts

# Install dependencies
apt-get update
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list
${credstash_install_snippet}
apt-get install -y awscli
apt-get install -y openjdk-8-jre
apt-get install -y elasticsearch=${elasticsearch_version}

# Mount EBS volume
${mount_snippet}

# Create file system if volume is empty.
if file -s ${device_name} | grep "${device_name}: data"; then
    echo "Creating file system on ${device_name}".
    mkfs -t ext4 ${device_name}
fi
mkdir ${mount_point}
echo "${device_name}       ${mount_point}   ext4    defaults  0 2" >> /etc/fstab
mount -a

/usr/share/elasticsearch/bin/elasticsearch-plugin install -b discovery-ec2
# Set JAVA heap to 50% of available RAM, but no more than 26Gb
MAX_HEAP=26624
JVM_HEAP_SIZE=$(grep MemTotal /proc/meminfo | \
                   awk -v max_heap=$MAX_HEAP \
                       '{$2/=(2*1024);printf "%dm\n",($2 > max_heap) ? max_heap : $2}')
sed -i -e "s/^-Xms.*/-Xms$JVM_HEAP_SIZE/g" /etc/elasticsearch/jvm.options
sed -i -e "s/^-Xmx.*/-Xmx$JVM_HEAP_SIZE/g" /etc/elasticsearch/jvm.options

echo "${config_yaml}" > /etc/elasticsearch/elasticsearch.yml
mkdir ${mount_point}/data ${mount_point}/logs
chown elasticsearch:elasticsearch ${mount_point}/data ${mount_point}/logs
#chmod 640 ${mount_point}/data ${mount_point}/logs
echo "path.data: ${mount_point}/data" >> /etc/elasticsearch/elasticsearch.yml
echo "path.logs: ${mount_point}/logs" >> /etc/elasticsearch/elasticsearch.yml
systemctl daemon-reload
systemctl enable elasticsearch.service
systemctl start elasticsearch.service

## ============
## Setup Beats
## ============

mkdir -p /etc/beats/ssl/

${credstash_get_cmd} -n ${credstash_ca_cert_name} ${credstash_context} > /etc/beats/ssl/ca.crt
${credstash_get_cmd} -n ${credstash_client_cert_name} ${credstash_context} > /etc/beats/ssl/client.crt
${credstash_get_cmd} -n ${credstash_client_key_name} ${credstash_context} > /etc/beats/ssl/client.key


## Setup Filebeat
## --------------

apt-get install -y filebeat

mkdir /etc/filebeat/prospectors/

cat <<EOF > /etc/filebeat/filebeat.yml
filebeat.config_dir: '/etc/filebeat/prospectors/'
filebeat.prospectors:
- input_type: log
  paths:
    - '/var/log/syslog'
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: aws
      source: elasticsearch
      formats:
        - syslog
      transport: filebeat
- input_type: log
  paths:
    - '${mount_point}/logs/elasticsearch*.log'
  multiline.pattern: '^\[\d{4}-\d{2}-\d{2}T[0-2][0-9]:[0-5][0-9]:[0-6][0-9](\,\d+)?\]'
  multiline.negate: true
  multiline.match: after
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: elk-elasticsearch-${is_master_node ? "master" : "data" }-node
      source: elasticsearch
      formats:
        - log4j2
      transport: filebeat
- input_type: log
  paths:
    - '/var/log/filebeat/filebeat*.log'
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: elk-elasticsearch-${is_master_node ? "master" : "data" }-node
      source: filebeat
      formats:
        - filebeat
      transport: filebeat
- input_type: log
  paths:
    - '/var/log/metricbeat/metricbeat*.log'
  fields_under_root: true
  fields:
    index_prefix: elk
    log_info:
      origin: elk-elasticsearch-${is_master_node ? "master" : "data" }-node
      source: metricbeat
      formats:
        - metricbeat
      transport: filebeat
output.logstash:
  hosts: ["${logstash_beats_address}"]
  ssl.certificate_authorities: ['/etc/beats/ssl/ca.crt']
  ssl.certificate: '/etc/beats/ssl/client.crt'
  ssl.key: '/etc/beats/ssl/client.key'
logging:
  level: info
  to_files: true
  to_syslog: false
  files:
    name: filebeat.log
EOF

systemctl daemon-reload
systemctl enable filebeat.service
systemctl start filebeat.service


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
    - 'curator'
  cpu_ticks: false
  fields_under_root: true
  fields:
    index_prefix: metricbeat-elk
    metric_info:
      origin: elk-elasticsearch-${is_master_node ? "master" : "data" }-node
      source: metricbeat
output.logstash:
  hosts: ["${logstash_beats_address}"]
  ssl.certificate_authorities: ['/etc/beats/ssl/ca.crt']
  ssl.certificate: '/etc/beats/ssl/client.crt'
  ssl.key: '/etc/beats/ssl/client.key'
logging:
  level: info
  to_files: true
  to_syslog: false
  files:
    name: metricbeat.log
EOF

curl -H 'Content-Type: application/json' -XPUT 'http://localhost:9200/_template/metricbeat' -d@/etc/metricbeat/metricbeat.template.json

systemctl daemon-reload
systemctl enable metricbeat.service
systemctl start metricbeat.service


${extra_setup_snippet}

