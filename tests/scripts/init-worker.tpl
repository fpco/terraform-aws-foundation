#!/bin/sh
# create a unique hostname
HN_PREFIX="worker"
INSTANCE_ID="`ec2metadata --instance-id`"
HOSTNAME="$HN_PREFIX-$INSTANCE_ID"
VERBOSE="--log-level=debug"
LOG_PREFIX="OPSTACK:"

# crack open a log!
echo "$LOG_PREFIX hello world from $HOSTNAME"
echo "$LOG_PREFIX welcome to the provisioning log from cloud init!"
echo "$LOG_PREFIX `date`"

echo "$LOG_PREFIX update salt minion id to $HOSTNAME"
echo $HOSTNAME > /etc/salt/minion_id

#########################################################
# Update Salt Pillar with details from this instance
cat <<EOT > /srv/pillar/bootstrap.sls
hostname: $HOSTNAME
consul:
  datacenter: ${region}
  secret_key: ${secret_key}
  leaders:
    - ${leader_dns}
  master_token: ${master_token}

consul_template:
  client_token: ${master_token}
EOT

# share the results
echo "$LOG_PREFIX wrote out the following /srv/pillar/bootstrap.sls:"
cat /srv/pillar/bootstrap.sls
#########################################################

# this should not be here forever..
#ufw allow 8888
ufw status numbered
#########################################################

echo "$LOG_PREFIX ensure /etc/hosts has our hostname"
sed -i "s/localhost/localhost $HOSTNAME/" /etc/hosts
echo "$LOG_PREFIX apply the hostname salt formula"
salt-call --local state.sls hostname $VERBOSE
echo "$LOG_PREFIX restart dnsmasq to be sure it is online"
service dnsmasq restart
echo "$LOG_PREFIX apply the consul.service salt formula, then pause while consul joins"
salt-call --local state.sls consul.service $VERBOSE
sleep 5
echo "$LOG_PREFIX restart salt-minion now that consul agent is online"
service salt-minion restart
echo "$LOG_PREFIX configure/restart consul-template service"
salt-call --local state.sls consul.template-tool.service $VERBOSE
