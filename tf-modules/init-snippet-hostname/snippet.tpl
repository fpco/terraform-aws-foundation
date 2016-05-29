# start snippet - update hostname
${init_prefix}
# create a unique hostname
HN_PREFIX="${hostname_prefix}"
INSTANCE_ID="`ec2metadata --instance-id`"
HOSTNAME="$HN_PREFIX-$INSTANCE_ID"
echo "${log_prefix} update salt minion id to $HOSTNAME"
echo $HOSTNAME > /etc/salt/minion_id

cat <<EOT >> ${bootstrap_pillar_file}
hostname: $HOSTNAME
EOT

echo "${log_prefix} ensure /etc/hosts has our hostname"
sed -i "s/localhost/localhost $HOSTNAME/" /etc/hosts
echo "${log_prefix} apply the hostname salt formula"
salt-call --local state.sls hostname --log-level=${log_level}
${init_suffix}
