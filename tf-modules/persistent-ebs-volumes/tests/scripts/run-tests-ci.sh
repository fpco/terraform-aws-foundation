#!/bin/sh
#
# Script to run tests, suitable for running with CI like jenkins/bamboo.
# Expects to be called with a single argument, the name of the test VPC
# deployed, and with 3 environment variables:
#
#  * AWS_SECRET_ACCESS_KEY
#  * AWS_ACCESS_KEY_ID
#  * AWS_DEFAULT_REGION
#
# The script also expects to be run from the parent path ("tests")

LOG_PREFIX="Persistent EBS Test:"

KEY_FILE="$(terraform output key_file)"
eval KEY_FILE=$KEY_FILE # this is needed to properly evaluate a tilde (if present)

SSH_CMD_PREFIX="ssh -i $KEY_FILE -o StrictHostKeyChecking=no"
echo "$LOG_PREFIX SSH command prefix looks like: $SSH_CMD_PREFIX"

#echo "$LOG_PREFIX going to pause for 600 seconds (10 minutes) while we wait for the clusters to come online"
#sleep 600

# these ASG names need to be changed in the module source -_-
ASG_NAME="$(terraform output asg_name)"

#######################################################################
#
echo "$LOG_PREFIX Lookup all details on Leader ASG"
aws autoscaling describe-auto-scaling-groups --auto-scaling-group-name $ASG_NAME > TEST_ASG.json
jq '.' < TEST_ASG.json

echo "$LOG_PREFIX collect instance IDs from Leader ASG - output as a single line text file"
jq '.AutoScalingGroups[0] | {instance_id: .Instances[].InstanceId}' < TEST_ASG.json | jq -s '.[] | .instance_id' | sed 's/"//g' | tr '\n' ' ' > TEST_IDS.txt
aws ec2 describe-instances \
  --instance-ids `cat TEST_IDS.txt` \
  --query "Reservations[*].Instances[*].PublicIpAddress" \
  --output=json \
  | jq 'flatten' > TEST_INSTANCE_IP.json
jq '.' < TEST_INSTANCE_IP.json

# grab the one IP and run a few tests
TEST_IP=$(jq '.[0]' < TEST_INSTANCE_IP.json | sed 's/"//g')

#echo "$LOG_PREFIX ping test leader and check it's member list:"
#$SSH_CMD_PREFIX ubuntu@$TEST_IP "hostname && sudo consul members"

#echo "$LOG_PREFIX here's the full leader cloud-init-output.log.."
#$SSH_CMD_PREFIX ubuntu@$TEST_IP "sudo cat /var/log/cloud-init-output.log" > TEST-cloud-init-output.log
#
#echo "$LOG_PREFIX directory listing.."
#ls -alh
