#!/bin/sh

echo "move /tmp/id_rsa > /root/.ssh/id_rsa"
mv /tmp/id_rsa /root/.ssh/id_rsa
echo "chown and chmod to correct ownership / permissions on /root/.ssh/id_rsa"
chown root:root /root/.ssh/id_rsa
chmod 600 /root/.ssh/id_rsa
