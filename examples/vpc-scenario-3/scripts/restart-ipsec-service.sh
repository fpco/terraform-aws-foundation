#!/usr/bin/env bash

# Restart ipsec
ssh -i remote-environment/id_rsa openvpnas@$(terraform output -state="remote-environment/terraform.tfstate" openvpn-public-eip) "sudo sudo systemctl restart openvpnas.service && sudo ipsec restart"

echo "Restarted ipsec"
