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

# Mount volume
MAX_SLEEP=60
TOTAL_SLEEP=0
VOLUME_ID=${volume_id}
INSTANCE_ID=$$(ec2metadata --instance-id)
echo "Attaching $$VOLUME_ID to instance $$INSTANCE_ID via the AWS API in ${region}"
aws --region=${region}          \
    ec2 attach-volume           \
    --volume-id $$VOLUME_ID     \
    --instance-id $$INSTANCE_ID \
    --device ${device_name}

ls ${device_name}
while [ $$? -ne 0 ]; do
  if [ "$$TOTAL_SLEEP" -gt "$$MAX_SLEEP" ]; then
    echo "Was unable to attach volume within limit time of $$MAX_SLEEP seconds."
    exit 1
  fi
  TOTAL_SLEEP=$$((TOTAL_SLEEP + ${wait_interval}))
  sleep ${wait_interval}
  ls ${device_name}
done

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
