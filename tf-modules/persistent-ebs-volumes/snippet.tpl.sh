# Mount volume
MAX_SLEEP=${max_wait}
TOTAL_SLEEP=0
VOLUME_ID=${volume_id}
INSTANCE_ID=$$(ec2metadata --instance-id)
REGION=$$(ec2metadata --availability-zone | sed 's/.$//')
echo "Attaching $$VOLUME_ID to instance $$INSTANCE_ID via the AWS API in $${REGION}"
aws --region=$${REGION}         \
    ec2 attach-volume           \
    --volume-id $$VOLUME_ID     \
    --instance-id $$INSTANCE_ID \
    --device ${device_name}

ls ${device_name}
while [ $$? -ne 0 ]; do
  if [ "$$TOTAL_SLEEP" -gt "$$MAX_SLEEP" ]; then
    echo "Was unable to attach volume within limit time of $$MAX_SLEEP seconds."
    exit 1
  fi
  TOTAL_SLEEP=$$((TOTAL_SLEEP + ${wait_interval}))
  sleep ${wait_interval}
  ls ${device_name}
done
