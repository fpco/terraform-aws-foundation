# start snippet - attach EBS volume
${init_prefix}
export AWS_DEFAULT_REGION=${region}
VOLUME_ID=${volume_id}
INSTANCE_ID="$(wget -O- http://169.254.169.254/latest/meta-data/instance-id || curl http://169.254.169.254/latest/meta-data/instance-id)"
echo "${log_prefix} will attach $${VOLUME_ID} via the AWS API in ${region}"
while ! aws ec2 attach-volume                     \
          --volume-id "$${VOLUME_ID}"     \
          --instance-id "$${INSTANCE_ID}" \
          --device '${device_path}'; do
  echo "Attaching command failed to run. Retrying."
  sleep '${wait_interval}'
done

while ! ls '${device_path}'; do
  sleep '${wait_interval}'
done
${init_suffix}
