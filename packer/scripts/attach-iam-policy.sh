#!/bin/sh

IAM_USER=$1
ARN=$2

aws iam attach-user-policy --user-name $IAM_USER \
                           --policy-arn $ARN

aws iam list-attached-user-policies --user-name $IAM_USER | jq .
