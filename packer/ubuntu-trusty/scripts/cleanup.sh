#!/usr/bin/env bash

set -eux

apt-get -y autoremove
apt-get -y clean

echo "clear consul's tmp path, if it is present"
rm -rf /home/consul/tmp/* || true
rm -rf /root/.ssh/id_rsa
rm -rf /home/ubuntu/.ssh/*
