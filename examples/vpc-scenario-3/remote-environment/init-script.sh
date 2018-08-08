#!/usr/bin/env bash

# Install Ipsec binaries
sudo apt-get update
sudo apt-get install -yq strongswan apparmor-utils
sudo curl -s -L -o /sbin/ipsec.sh  https://docs.openvpn.net/wp-content/uploads/ipsec.sh
sudo chmod +x /sbin/ipsec.sh

# Fix bug permission bug with ipsec https://bugs.launchpad.net/ubuntu/+source/strongswan/+bug/1587886
sudo aa-complain /usr/lib/ipsec/charon
sudo aa-complain /usr/lib/ipsec/stroke

# Setup openvpn server
sudo /usr/local/openvpn_as/bin/ovpn-init tool --force --batch
echo -e "openvpn\nopenvpn" | sudo passwd openvpn
sudo ipsec status
