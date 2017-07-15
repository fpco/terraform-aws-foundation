# start snippet - update ubuntu hostname (simple)
${init_prefix}
# create a unique hostname
HN_PREFIX="${hostname_prefix}"
INSTANCE_ID="`ec2metadata --instance-id`"
HOSTNAME="$HN_PREFIX-$INSTANCE_ID"
hostnamectl set-hostname $HOSTNAME
echo "${log_prefix} ensure /etc/hosts has our hostname"
sed -i "s/localhost/localhost $HOSTNAME/" /etc/hosts
#echo $HOSTNAME > /etc/hostname
#service hostname restart
${init_suffix}
