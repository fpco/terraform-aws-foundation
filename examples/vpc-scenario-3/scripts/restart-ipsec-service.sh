#!/usr/bin/env bash

# Restart ipsec
ssh -i remote-environment/id_rsa openvpnas@$(terraform output -state="remote-environment/terraform.tfstate" openvpn-public-eip) "sudo ipsec restart" 2&>1

echo "Restarted ipsec"
