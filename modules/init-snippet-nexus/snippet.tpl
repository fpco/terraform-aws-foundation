mkdir -p ${mount_path}
mount ${device_path} ${mount_path}

cp /etc/fstab /etc/fstab.orig
echo "LABEL=${device_label}            ${mount_path}  ext4   defaults,nofail     0 2" >> /etc/fstab

apt-get install -y docker docker.io
cat >/etc/rc.local <<EOL
#!/bin/bash
set -e
set -x
mkdir -p ${data_path}
chown -R 200 ${data_path}
docker run \
  --detach \
  --publish ${port}:8081 \
  --restart always \
  --volume ${data_path}/sonatype-work \
  sonatype/nexus
EOL
/etc/rc.local
