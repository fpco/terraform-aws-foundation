#/usr/bin/env bash

cat << EOF > remote-environment/ipsec.conf
# Amazon VPC IPsec configuration for the OpenVPN Access Server Appliance
conn %default
    left=%any
    keyexchange=ikev1
    keyingtries=%forever
    esp=aes128-sha1-modp1024
    ike=aes128-sha1-modp1024
    ikelifetime=8h
    auto=start
    authby=secret
    dpdaction=restart
    closeaction=restart
    dpddelay=10s
    dpdtimeout=30s
    leftsubnet=0.0.0.0/0
    leftupdown=/sbin/ipsec.sh
    installpolicy=no
    # Enter your VPC subnet here (in CIDR format - e.g. rightsubnet=10.0.0.0/16)
    rightsubnet=$(terraform output vpc_cidr)

conn VPC-CUST-GW1
    # Enter the tunnel 1 endpoint here (e.g. right=205.251.233.121)
    right=$(terraform output vpn_tun1_ip)

conn VPC-CUST-GW2
    # Enter the tunnel 2 endpoint here (e.g. right=205.251.233.122)
    right=$(terraform output vpn_tun2_ip)
EOF

cat << EOF > remote-environment/ipsec.secrets
$(terraform output vpn_tun1_ip) : PSK "$(terraform output vpn_tun1_key)"
$(terraform output vpn_tun2_ip) : PSK "$(terraform output vpn_tun2_key)"
EOF
