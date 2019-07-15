# start snippet - attach EBS volume
${init_prefix}
export AWS_DEFAULT_REGION=${region}
VOLUME_ID=${volume_id}
INSTANCE_ID="$(ec2metadata --instance-id)"
echo "${log_prefix} will attach $${VOLUME_ID} via the AWS API in ${region}"
aws ec2 attach-volume                     \
          --volume-id "$${VOLUME_ID}"     \
          --instance-id "$${INSTANCE_ID}" \
          --device '${device_path}'

while ! ls '${device_path}'; do
  sleep '${wait_interval}'
done
${init_suffix}