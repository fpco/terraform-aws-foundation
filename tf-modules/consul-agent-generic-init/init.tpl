#!/bin/sh

# create a unique hostname
HN_PREFIX="${service}"
INSTANCE_ID="`ec2metadata --instance-id`"
HOSTNAME="$HN_PREFIX-$INSTANCE_ID"
VERBOSE="--log-level=${log_level}"

# crack open a log!
echo "hello world from $HOSTNAME"
echo "welcome to the provisioning log from cloud init!"
echo `date`

echo "update salt minion id to $HOSTNAME"
echo $HOSTNAME > /etc/salt/minion_id

#########################################################
# Update Salt Pillar with details from this instance
cat <<EOT > /srv/pillar/bootstrap.sls
# written during cloud-init
hostname: $HOSTNAME

# setup consul
consul:
  client_token: ${consul_client_token}
  datacenter: ${region}
  secret_key: ${consul_secret_key}
  leaders:
    - ${leader_dns}
  webui: True

consul_template:
  client_token: ${consul_client_token}

${extra_pillar}
EOT

# share the results
echo "wrote out the following /srv/pillar/bootstrap.sls:"
cat /srv/pillar/bootstrap.sls
#########################################################

APPLY_FORMULA="salt-call --local state.sls"
echo "ensure /etc/hosts has our hostname"
sed -i "s/localhost/localhost $HOSTNAME/" /etc/hosts
echo "apply the hostname salt formula"
$APPLY_FORMULA hostname $VERBOSE
echo "restart dnsmasq to be sure it is online for consul"
service dnsmasq restart
echo "apply the consul.agent salt formula, then pause while consul joins"
$APPLY_FORMULA consul.service $VERBOSE
sleep 5
echo "apply consul-template formula to configure/run service"
$APPLY_FORMULA consul.template-tool.service $VERBOSE
${extra_init}
