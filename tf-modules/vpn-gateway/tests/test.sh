#!/bin/sh

# Deploy VPC, ec2 instance running `vpn-gateway` service and ec2-instance within
# the same VPC and validate that connection to internal sites, which are available
# on VPN.

SSH_KEY=ssh_key
if [ ! -f $SSH_KEY ]; then
  ssh-keygen -q -N "" -f $SSH_KEY
fi

terraform get
terraform plan
terraform apply

HOSTED_ZONE_ID="$(terraform output route53_zone_id)"
DOMAINS=$(aws route53 list-resource-record-sets --hosted-zone-id=$HOSTED_ZONE_ID | jq '.ResourceRecordSets[] | select(.Type == "A") | .Name')
REMOTE_CMD=""
for domain in $DOMAINS; do
  if [ -z "$REMOTE_CMD" ]; then
    REMOTE_CMD="ping -c 3 $domain"
  else
    REMOTE_CMD="${REMOTE_CMD} && ping -c 3 $domain"
  fi
done
KNOWN_HOSTS=$(mktemp "/tmp/XXX-known_hosts")

echo "Testing VPN gateway"
VPN_GATEWAY_IP="$(terraform output vpn_gateway_public_ip)"
ssh-keyscan "$VPN_GATEWAY_IP" > "$KNOWN_HOSTS"
ssh -i $SSH_KEY -o UserKnownHostsFile="$KNOWN_HOSTS" "ubuntu@$VPN_GATEWAY_IP" \
    "sudo service vpn-gateway status && ${REMOTE_CMD} && sudo service vpn-gateway restart && ${REMOTE_CMD}"

echo "Testing access to VPN through the gateway"
TEST_INSTANCE_IP="$(terraform output test_instance_public_ip)"
ssh-keyscan "$TEST_INSTANCE_IP" >> "$KNOWN_HOSTS"
ssh -i $SSH_KEY -o UserKnownHostsFile="$KNOWN_HOSTS" "ubuntu@$TEST_INSTANCE_IP" \
    "${REMOTE_CMD}"
rm -f "$KNOWN_HOSTS"

terraform destroy -force
