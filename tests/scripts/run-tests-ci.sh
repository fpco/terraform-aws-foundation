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

KEY_FILE="$(terraform output key_file)"
KEY_NAME="$(terraform output key_name)"
eval KEY_FILE=$KEY_FILE # this is needed to properly evaluate a tilde (if present)

SSH_CMD_PREFIX="ssh -i $KEY_FILE -o StrictHostKeyChecking=no"
echo "$LOG_PREFIX SSH command prefix looks like: $SSH_CMD_PREFIX"

echo "$LOG_PREFIX going to pause for 600 seconds (10 minutes) while we wait for the clusters to come online"
sleep 600

# these ASG names need to be changed in the module source -_-
LEADER_ASG_NAME="$(terraform output leader_asg_name)"
WORKER_ASG_NAME="$(terraform output worker_asg_name)"
MANAGE_ASG_NAME="$(terraform output manage_asg_name)"

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
$SSH_CMD_PREFIX ubuntu@$TEST_LEADER_IP "sudo cat /var/log/cloud-init-output.log" > leader-cloud-init-output.log

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
$SSH_CMD_PREFIX ubuntu@$TEST_WORKER_IP "sudo cat /var/log/cloud-init-output.log" > worker-cloud-init-output.log

#######################################################################
# elasticache
EC_URL=$(terraform output elasticache_url)
echo "$LOG_PREFIX looked up elasticache url, will now test $EC_URL with netcat -vz"
$SSH_CMD_PREFIX ubuntu@$TEST_WORKER_IP "nc -vz $EC_URL 6379"

#######################################################################
# management
echo "$LOG_PREFIX Lookup all details on Management ASG"
aws autoscaling describe-auto-scaling-groups --auto-scaling-group-name $MANAGE_ASG_NAME > MANAGE_ASG.json
jq '.' < MANAGE_ASG.json

echo "$LOG_PREFIX collect instance IDs from Management ASG and look up IPs"
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
$SSH_CMD_PREFIX ubuntu@$TEST_MANAGE_IP "sudo cat /var/log/cloud-init-output.log" > manage-cloud-init-output.log

#######################################################################
# nat module
BASTION_DNS="$(terraform output bastion_dns)"
PRIVATE_INSTANCE_DNS="$(terraform output private_instance_dns)"
BASTION_SSH="ssh -o StrictHostKeyChecking=no -A -i $KEY_FILE ubuntu@$BASTION_DNS -W %h:%p"

echo "ping google from our test instance behind NAT, in a private subnet.."
echo "use $KEY_NAME and $KEY_FILE to tunnel through $BASTION_DNS to hit $PRIVATE_INSTANCE_DNS.."
echo "Here is the EC2 instance in the private subnet:"
ssh -o StrictHostKeyChecking=no    \
    -o ProxyCommand="$BASTION_SSH" \
    -i $KEY_FILE                   \
       ubuntu@$PRIVATE_INSTANCE_DNS 'hostname && uname -a && ping -c 1 google.com'

# DONE!

echo "$LOG_PREFIX directory listing.."
ls -alh
