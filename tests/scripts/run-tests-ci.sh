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

LOG_PREFIX="OPSTACK:"

echo "$LOG_PREFIX going to pause for 450 seconds (7.5 minutes) while we wait for the clusters to come online"
sleep 450

VPC_NAME="$1"
SSH_CMD_PREFIX="ssh -i ./id_rsa -o StrictHostKeyChecking=no"
echo "$LOG_PREFIX SSH command prefix looks like: $SSH_CMD_PREFIX"

# these ASG names need to be changed in the module source -_-
LEADER_ASG_NAME="leaders.consul.$VPC_NAME"
WORKER_ASG_NAME="$VPC_NAME.consul-cluster"
MANAGE_ASG_NAME="$VPC_NAME-management-tier.consul-cluster"

#######################################################################
# leaders
echo "$LOG_PREFIX Lookup all details on Leader ASG"
aws autoscaling describe-auto-scaling-groups --auto-scaling-group-name $LEADER_ASG_NAME > LEADER_ASG.json
jq '.' < LEADER_ASG.json

echo "$LOG_PREFIX collect instance IDs from Leader ASG - output as a single line text file"
jq '.AutoScalingGroups[0] | {instance_id: .Instances[].InstanceId}' < LEADER_ASG.json | jq -s '.[] | .instance_id' | sed 's/"//g' | tr '\n' ' ' > LEADER_IDS.txt
aws ec2 describe-instances \
  --instance-ids `cat LEADER_IDS.txt` \
  --query "Reservations[*].Instances[*].PublicIpAddress" \
  --output=json \
  | jq 'flatten' > LEADER_INSTANCE_IPS.json
jq '.' < LEADER_INSTANCE_IPS.json

# grab one IP from the cluster of leaders and run a few tests
TEST_LEADER_IP=$(jq '.[0]' < LEADER_INSTANCE_IPS.json | sed 's/"//g')

echo "$LOG_PREFIX ping test leader and check it's member list:"
$SSH_CMD_PREFIX ubuntu@$TEST_LEADER_IP "hostname && sudo consul members"

echo "$LOG_PREFIX here's the full leader cloud-init-output.log.."
$SSH_CMD_PREFIX ubuntu@$TEST_LEADER_IP "sudo cat /var/log/cloud-init-output.log"

#######################################################################
# workers
echo "$LOG_PREFIX Lookup all details on Worker ASG"
aws autoscaling describe-auto-scaling-groups --auto-scaling-group-name $WORKER_ASG_NAME > WORKER_ASG.json
jq '.' < WORKER_ASG.json

echo "$LOG_PREFIX collect instance IDs from Worker ASG - output as a single line text file"
jq '.AutoScalingGroups[0] | {instance_id: .Instances[].InstanceId}' < WORKER_ASG.json | jq -s '.[] | .instance_id' | sed 's/"//g' | tr '\n' ' ' > WORKER_IDS.txt
aws ec2 describe-instances \
  --instance-ids `cat WORKER_IDS.txt` \
  --query "Reservations[*].Instances[*].PublicIpAddress" \
  --output=json \
  | jq 'flatten' > WORKER_INSTANCE_IPS.json
jq '.' < WORKER_INSTANCE_IPS.json

# grab one IP from the cluster of workers and run a few tests
TEST_WORKER_IP=$(jq '.[0]' < WORKER_INSTANCE_IPS.json | sed 's/"//g')

echo "$LOG_PREFIX ping test worker and check it's member list:"
$SSH_CMD_PREFIX ubuntu@$TEST_WORKER_IP "hostname && sudo consul members"

echo "$LOG_PREFIX here's the full worker cloud-init-output.log.."
$SSH_CMD_PREFIX ubuntu@$TEST_WORKER_IP "sudo cat /var/log/cloud-init-output.log"

#######################################################################
# elasticache
EC_URL=$(terraform output elasticache_url)
echo "$LOG_PREFIX looked up elasticache url, will now test $EC_URL with netcat -vz"
$SSH_CMD_PREFIX ubuntu@$TEST_WORKER_IP "nc -vz $EC_URL 6379"

#######################################################################
# management
echo "$LOG_PREFIX Lookup all details on Worker ASG"
aws autoscaling describe-auto-scaling-groups --auto-scaling-group-name $MANAGE_ASG_NAME > MANAGE_ASG.json
jq '.' < MANAGE_ASG.json

echo "$LOG_PREFIX collect instance IDs from Worker ASG - output as a single line text file"
jq '.AutoScalingGroups[0] | {instance_id: .Instances[].InstanceId}' < MANAGE_ASG.json | jq -s '.[] | .instance_id' | sed 's/"//g' | tr '\n' ' ' > MANAGE_IDS.txt
aws ec2 describe-instances \
  --instance-ids `cat MANAGE_IDS.txt` \
  --query "Reservations[*].Instances[*].PublicIpAddress" \
  --output=json \
  | jq 'flatten' > MANAGE_INSTANCE_IPS.json
jq '.' < MANAGE_INSTANCE_IPS.json

# grab one IP from the cluster of manages and run a few tests
TEST_MANAGE_IP=$(jq '.[0]' < MANAGE_INSTANCE_IPS.json | sed 's/"//g')

echo "$LOG_PREFIX ping test manage and check it's member list:"
$SSH_CMD_PREFIX ubuntu@$TEST_MANAGE_IP "hostname && sudo consul members && sudo service openntpd status"

echo "$LOG_PREFIX here's the full manage cloud-init-output.log.."
$SSH_CMD_PREFIX ubuntu@$TEST_MANAGE_IP "sudo cat /var/log/cloud-init-output.log"


#######################################################################
# DONE!

echo "$LOG_PREFIX directory listing.."
ls -alh

