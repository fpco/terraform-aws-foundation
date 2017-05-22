#!/bin/bash

# Install dependencies
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | apt-key add -
apt-get install -y apt-transport-https
echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | tee -a /etc/apt/sources.list.d/elastic-5.x.list
apt-get update -q
apt-get install -y awscli
apt-get install -y \
        openjdk-8-jre \
        elasticsearch

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
echo "path.data: ${mount_point}/data" >> /etc/elasticsearch/elasticsearch.yml
echo "path.logs: ${mount_point}/logs" >> /etc/elasticsearch/elasticsearch.yml
systemctl daemon-reload
systemctl enable elasticsearch.service
systemctl start elasticsearch.service

# Install curator (will only run on active master)

pip install elasticsearch-curator
useradd -r curator
mkdir /var/log/curator/
chown curator:curator /var/log/curator
mkdir /etc/curator
TMP_CRON=$(mktemp -t "curator-cron-job-XXXXXX.txt")
echo '30 1 * * * /usr/local/bin/curator --config /etc/curator/config.yaml /etc/curator/actionfile.yaml' > $TMP_CRON
crontab -u curator $TMP_CRON

cat <<EOF > /etc/curator/config.yaml
client:
  hosts:
    - localhost
  port: 9200
  url_prefix:
  use_ssl: False
  certificate:
  client_cert:
  client_key:
  ssl_no_validate: False
  http_auth:
  timeout: 30
  master_only: True

logging:
  loglevel: INFO
  logfile: /var/log/curator/curator.log
  logformat: default
  blacklist: ['elasticsearch', 'urllib3']
EOF

cat <<EOF > /etc/curator/actionfile.yaml
actions:
  1:
    action: delete_indices
    description: >-
      Delete indices older than 60 days (based on index name), for filebeat-
      prefixed indices. Ignore the error if the filter does not result in an
      actionable list of indices (ignore_empty_list) and exit cleanly.
    options:
      ignore_empty_list: True
      timeout_override:
      continue_if_exception: False
      disable_action: False
    filters:
    - filtertype: pattern
      kind: prefix
      value: filebeat-
      exclude:
    - filtertype: age
      source: name
      direction: older
      timestring: '%Y.%m.%d'
      unit: days
      unit_count: 60
      exclude:
EOF
