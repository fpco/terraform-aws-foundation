# start snippet - attach EBS volume
${init_prefix}
export AWS_DEFAULT_REGION=${region}
VOLUME_ID=${volume_id}
if which wget; then
  INSTANCE_ID="$(wget -O- http://169.254.169.254/latest/meta-data/instance-id)"
elif which curl; then
  INSTANCE_ID="$(curl http://169.254.169.254/latest/meta-data/instance-id)"
fi

if [ "x$${INSTANCE_ID}" == "x" ]; then
  echo 'OS not functioning'
else
  echo "${log_prefix} will attach $${VOLUME_ID} via the AWS API in ${region}"
  while ! aws ec2 attach-volume                     \
          --volume-id "$${VOLUME_ID}"     \
          --instance-id "$${INSTANCE_ID}" \
          --device '${device_path}'; do
    echo "Attaching command failed to run. Retrying."
    sleep '${wait_interval}'
  done
  echo "${log_prefix} $${VOLUME_ID} attached."
  
  vol_id="$(echo "$${VOLUME_ID}" | tr -d '-')"
  while [ ! -e /dev/disk/by-id/*-Amazon_Elastic_Block_Store_$${vol_id} ]; do
    sleep '${wait_interval}' 
  done
  
  dev_id="$(ls /dev/disk/by-id/*-Amazon_Elastic_Block_Store_$${vol_id} | head -1)"
  dev_name="/dev/$(readlink "$${dev_id}" | tr / '\n' | tail -1)"
  [ "$${dev_name}" == "${device_path}" ] || ln -s "$${dev_name}" "${device_path}"
fi

${init_suffix}
