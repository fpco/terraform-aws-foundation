#!/bin/sh
# create a unique hostname
HN_PREFIX="leaders"
INSTANCE_ID="`ec2metadata --instance-id`"
HOSTNAME="$HN_PREFIX-$INSTANCE_ID"
VERBOSE="--log-level=debug"
PRIVATE_IP="`ec2metadata --local-ipv4`"
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
# written during cloud-init
hostname: $HOSTNAME

# setup consul
consul:
  datacenter: ${region}
  secret_key: '${secret_key}'
  master_token: '${master_token}'
  client_token: '${client_token}'
  leader_count: '${leader_count}'
  # can we shorten the list? check the CIDR math
  leaders:
    - ${cidr_prefix_a}.5
    - ${cidr_prefix_c}.5
    - ${cidr_prefix_a}.6
    - ${cidr_prefix_c}.6
    - ${cidr_prefix_a}.7
    - ${cidr_prefix_c}.7
    - ${cidr_prefix_a}.8
    - ${cidr_prefix_c}.8
    - ${cidr_prefix_a}.9
    - ${cidr_prefix_c}.9
    - ${cidr_prefix_a}.10
    - ${cidr_prefix_c}.10
    - ${cidr_prefix_a}.11
    - ${cidr_prefix_c}.11
    - ${cidr_prefix_a}.12
    - ${cidr_prefix_c}.12
    - ${cidr_prefix_a}.13
    - ${cidr_prefix_c}.13
    - ${cidr_prefix_a}.14
    - ${cidr_prefix_c}.14
    - ${cidr_prefix_a}.15
    - ${cidr_prefix_c}.15
    - ${cidr_prefix_a}.16
    - ${cidr_prefix_c}.16
    - ${cidr_prefix_a}.17
    - ${cidr_prefix_c}.17
    - ${cidr_prefix_a}.18
    - ${cidr_prefix_c}.18
    - ${cidr_prefix_a}.19
    - ${cidr_prefix_c}.19
    - ${cidr_prefix_a}.20
    - ${cidr_prefix_c}.20
    - ${cidr_prefix_a}.21
    - ${cidr_prefix_c}.21
    - ${cidr_prefix_a}.22
    - ${cidr_prefix_c}.22
    - ${cidr_prefix_a}.23
    - ${cidr_prefix_c}.23
    - ${cidr_prefix_a}.24
    - ${cidr_prefix_c}.24
    - ${cidr_prefix_a}.25
    - ${cidr_prefix_c}.25
    - ${cidr_prefix_a}.26
    - ${cidr_prefix_c}.26
    - ${cidr_prefix_a}.27
    - ${cidr_prefix_c}.27
    - ${cidr_prefix_a}.28
    - ${cidr_prefix_c}.28
    - ${cidr_prefix_a}.29
    - ${cidr_prefix_c}.29
    - ${cidr_prefix_a}.30
    - ${cidr_prefix_c}.30

consul_template:
  consul_addr: $PRIVATE_IP:8500
  client_token: ${client_token}
EOT

# share the results
echo "$LOG_PREFIX wrote out the following /srv/pillar/bootstrap.sls:"
cat /srv/pillar/bootstrap.sls
#########################################################

# this should not be here forever..
ufw disable
ufw status verbose
#########################################################

echo "$LOG_PREFIX ensure /etc/hosts has our hostname"
sed -i "s/localhost/localhost $HOSTNAME/" /etc/hosts
echo "$LOG_PREFIX apply the hostname salt formula"
salt-call --local state.sls hostname $VERBOSE
echo "$LOG_PREFIX restart dnsmasq to be sure it is online"
service dnsmasq restart
echo "$LOG_PREFIX apply the consul.service salt formula to run a leader"
salt-call --local state.sls consul.service $VERBOSE
echo "$LOG_PREFIX pause while consul joins.."
sleep 5
echo "$LOG_PREFIX restart salt-minion now that consul agent is online"
service salt-minion restart
echo "$LOG_PREFIX configure/restart consul-template service"
salt-call --local state.sls consul.template-tool.service $VERBOSE
