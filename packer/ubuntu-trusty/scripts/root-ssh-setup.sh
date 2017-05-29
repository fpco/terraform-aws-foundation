#!/bin/sh

set -ex
mv /tmp/id_rsa /root/.ssh/id_rsa
chown root:root /root/.ssh/id_rsa
chmod 600 /root/.ssh/id_rsa
ls -Alh /root/.ssh/
