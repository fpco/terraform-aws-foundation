#!/usr/bin/env bash

set -o pipefail

REGION=${1?}

aws acm import-certificate \
	--certificate file://gitlab.pem \
	--private-key file://gitlab-key.pem \
	--certificate-chain file://ca.pem \
	--region "${REGION}" |
jl '_.CertificateArn' |
tr -d '"' >upload-gen-cert-arn.txt
