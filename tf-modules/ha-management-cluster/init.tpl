#!/bin/sh

# create a unique hostname
HN_PREFIX="manage"
INSTANCE_ID="`ec2metadata --instance-id`"
HOSTNAME="$HN_PREFIX-$INSTANCE_ID"
VERBOSE="--log-level=debug"

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
  client_token: ${consul_master_token}
  datacenter: ${region}
  secret_key: ${consul_secret_key}
  leaders:
    - ${leader_dns}
  webui: True

consul_template:
  client_token: ${consul_master_token}

openntpd:
  server: True
EOT

# share the results
echo "wrote out the following /srv/pillar/bootstrap.sls:"
cat /srv/pillar/bootstrap.sls
#########################################################

echo "ensure /etc/hosts has our hostname"
sed -i "s/localhost/localhost $HOSTNAME/" /etc/hosts
echo "apply the hostname salt formula"
salt-call --local state.sls hostname $VERBOSE
echo "restart dnsmasq to be sure it is online"
service dnsmasq restart
echo "apply the consul.agent salt formula, then pause while consul joins"
salt-call --local state.sls consul.service $VERBOSE
sleep 5
echo "restart salt-minion now that consul agent is online"
service salt-minion restart
echo "setup openntpd in server mode"
salt-call --local state.sls openntpd $VERBOSE
echo "restart consul-template service"
service consul-template restart
