#!/bin/sh

echo "This script uploads an IAM policy for packer, and confirms what is uploaded"
echo "You should first run 'scripts/write-iam-policy.sh' to create 'ci-iam.json'"

POLICY_NAME=packer
POLICY_FILE=ci-iam.json
DESCRIPTION="Grants jenkins/packer limited access to VPC/subnet/EC2/EBS for creating AMIs"
POLICY_RESP=$(aws iam create-policy --policy-name $POLICY_NAME   \
                                    --description "$DESCRIPTION" \
                                    --policy-document file://$POLICY_FILE)

POLICY_ARN=$(echo $POLICY_RESP | jq '.Policy.Arn' | sed s/\"//g)
POLICY_VER=$(echo $POLICY_RESP | jq '.Policy.DefaultVersionId' | sed s/\"//g)


echo "Policy Uploaded:"
echo ""
echo $POLICY_RESP | jq .
echo ""

echo "--------------------------------"
echo "Contents of Uploaded Policy:"
echo ""

aws iam get-policy-version --policy-arn $POLICY_ARN \
	                   --version-id $POLICY_VER   | jq .

echo "now run 'scripts/attach-iam-policy.sh' with the name of the IAM user and"
echo "the following ARN as arguments: $POLICY_ARN"
