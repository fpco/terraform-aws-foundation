#!/usr/bin/env bash

#Start  ipsec
# Wait 20s for the service openvpn-as
echo "Restart openvpnas and ipsec"
ssh -i remote-environment/id_rsa openvpnas@$(terraform output -state="remote-environment/terraform.tfstate" openvpn-public-eip) "sudo service openvpnas restart && sleep 20 && sudo ipsec restart"
echo "Restarted ipsec"
