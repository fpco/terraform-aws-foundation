#!/usr/bin/env bash

set -o pipefail

# Obtain the public IP address for the given server and region

export SERVER_NAME=`terraform output server_name`
export REGION=`terraform output region`

# Use a here-document for variable interpolation to avoid having to escape double-quotes
FILTERS=$(
	cat <<-EOF
		[
			{
				"Name": "tag:aws:autoscaling:groupName",
				"Values": ["$SERVER_NAME"]
			},
			{
				"Name": "instance-state-name",
				"Values": ["running"]
			}
		]
	EOF
)
export SERVER_IP_1=`aws ec2 describe-instances --filters "$FILTERS" --region="${REGION}" | jl '_.Reservations[0].Instances[0].PublicIpAddress' | tr -d '"'`
export SERVER_IP_2=`aws ec2 describe-instances --filters "$FILTERS" --region="${REGION}" | jl '_.Reservations[1].Instances[0].PublicIpAddress' | tr -d '"'`

envsubst <ssh_config.tpl >ssh_config
