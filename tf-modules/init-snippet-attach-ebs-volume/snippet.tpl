# start snippet - attach EBS volume
${init_prefix}
export AWS_DEFAULT_REGION=${region}
VOLUME_ID=${volume_id}
INSTANCE_ID=$$(ec2metadata --instance-id)
echo "${log_prefix} will attach $$VOLUME_ID via the AWS API in ${region}"
aws ec2 attach-volume                \
          --volume-id $$VOLUME_ID     \
          --instance-id $$INSTANCE_ID \
          --device ${device_path}

ls ${device_path}
while [ $$? -ne 0 ]; do
  sleep ${wait_interval}
  ls ${device_path}
done
${init_suffix}
