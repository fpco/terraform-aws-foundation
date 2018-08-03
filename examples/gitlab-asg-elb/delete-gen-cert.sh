#!/usr/bin/env bash

set -o pipefail

REGION=${1?}

aws acm delete-certificate \
	--certificate-arn "$(cat upload-gen-cert-arn.txt)" \
	--region "${REGION}"
