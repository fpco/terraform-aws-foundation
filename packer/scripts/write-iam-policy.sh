#!/bin/sh

IAM_FILE="ci-iam.json"

echo "this script expects you to define the following environment variables:"
echo "  * REGION     - The region where the packer VPC lives"
echo "  * ACCOUNT_ID - AWS ARN for account"
echo "  * VPC_ID     - VPC for packer to create EC2 instances in"
echo "  * SUBNET_ID  - Subnet for packer to create EC2 instances in"
echo ""
echo "the IAM template will be written out to $IAM_FILE"

cat <<EOT > $IAM_FILE
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "NonResourceLevelPermissions",
            "Action": [
                "ec2:Describe*",
                "ec2:CreateVolume",
                "ec2:CreateKeypair",
                "ec2:DeleteKeypair",
                "ec2:CreateSecurityGroup",
                "ec2:AuthorizeSecurityGroupIngress",
                "ec2:CreateImage",
                "ec2:CreateSnapshot",
                "ec2:DeleteSnapshot",
                "ec2:RegisterImage",
                "ec2:CreateTags",
                "ec2:ModifyImageAttribute"
            ],
            "Effect": "Allow",
            "Resource": "*"
        },
        {
            "Sid": "AllowInstanceActions",
            "Effect": "Allow",
            "Action": [
                "ec2:StopInstances",
                "ec2:TerminateInstances",
                "ec2:AttachVolume",
                "ec2:DetachVolume",
                "ec2:DeleteVolume"
            ],
            "Resource": [
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:instance/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:volume/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:security-group/*"
            ],
            "Condition": {
                "StringEquals": {
                    "ec2:ResourceTag/Name": "Packer Builder"
                }
            }
        },
        {
            "Sid": "EC2RunInstancesSubnet",
            "Effect": "Allow",
            "Action": [
                "ec2:RunInstances"
            ],
            "Resource": [
                "arn:aws:ec2:$REGION::image/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:key-pair/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:network-interface/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:security-group/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:volume/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:instance/*",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:subnet/$SUBNET_ID",
                "arn:aws:ec2:$REGION:$ACCOUNT_ID:vpc/vpc-*"
            ]
        },
        {
            "Sid": "SGVPCDelete",
            "Effect": "Allow",
            "Action": [
                "ec2:DeleteSecurityGroup"
            ],
            "Resource": [
                "*"
            ],
            "Condition": {
                "StringEquals": {
                    "ec2:vpc": [
                        "arn:aws:ec2:$REGION:$ACCOUNT_ID:vpc/$VPC_ID"
                    ]
                }
            }
        }
    ]
}
EOT
