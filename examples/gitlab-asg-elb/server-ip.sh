#!/usr/bin/env bash

set -o pipefail

# Obtain the public IP address for the given server and region

SERVER_NAME=${1?} REGION=${2?}

# Use a here-document for variable interpolation to avoid having to escape double-quotes
FILTERS=$(
	cat <<-EOF
		[
			{
				"Name": "tag:Name",
				"Values": ["$SERVER_NAME"]
			},
			{
				"Name": "instance-state-name",
				"Values": ["running"]
			}
		]
	EOF
)

aws ec2 describe-instances --filters "$FILTERS" --region="${REGION}" |
jl '_.Reservations[0].Instances[0].PublicIpAddress' |
tr -d '"'
