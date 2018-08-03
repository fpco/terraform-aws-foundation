#!/usr/bin/env bash

rsync -e "ssh -i remote-environment/id_rsa" --rsync-path="sudo rsync" remote-environment/ipsec.conf openvpnas@$(terraform output -state="remote-environment/terraform.tfstate" openvpn-public-eip):/etc/
echo "Copy ipsec.conf to remote openvpn"

rsync -e "ssh -i remote-environment/id_rsa" --rsync-path="sudo rsync" remote-environment/ipsec.secrets openvpnas@$(terraform output -state="remote-environment/terraform.tfstate" openvpn-public-eip):/etc/
echo "Copy ipsec.conf to remote openvpn"

ssh -i remote-environment/id_rsa openvpnas@$(terraform output -state="remote-environment/terraform.tfstate" openvpn-public-eip) "sudo ipsec restart"
echo "Restart ipsec"
