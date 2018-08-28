# Hardcoding value of 3 minutes when we check if the plan file is stale
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`
SHELL := /bin/bash

## Check if tf.out is stale (Older than 2 minutes)
check-plan-file:
	@if ! ${STALE_PLAN_FILE} ; then \
		echo "ERROR: Stale tf.out plan file (older than 3 minutes)!"; \
		exit 1; \
	fi

## Runs terraform get and terraform init for env
init:
	@terraform get
	@terraform init

## tergaform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

## Plan the plan and apply it
plan-apply: init plan apply

## Generate terraform plan using `kops`
generate-kops-plan: init plan apply
	@export AWS_AVAILABILITY_ZONE="$(terraform output -json | jq '.availability_zone.value' | tr -d '\"')"
	@export KOPS_STATE_STORE="s3://$(terraform output -json | jq '.state_bucket_id.value' | tr -d '\"')"
	@export KOPS_CLUSTER_NAME="$(terraform output -json | jq '.subdomain_zone_name.value' | tr -d '\"')"
	@export KOPS_AWS_VPC="$(terraform output -json | jq '.vpc_id.value' | tr -d '\"')"
	@echo "Done exporting..."
	@echo "VPC ID:" ${KOPS_AWS_VPC}
	@echo "Press ENTER to continue"
	@read _input
	@echo ${KOPS_CLUSTER_NAME} ${AWS_AVAILABILITY_ZONE} ${KOPS_STATE_STORE} ${KOPS_AWS_VPC}
	@kops create cluster \
		--name=${KOPS_CLUSTER_NAME} \
		--zones=${AWS_AVAILABILITY_ZONE} \
		--state ${KOPS_STATE_STORE} \
		--master-size="t2.micro" \
		--node-size="t2.micro" \
		--node-count 2 \
		--vpc ${KOPS_AWS_VPC} \
		--target=terraform

##  aws-env -p fpco-dev-sandbox-admin -- kops create cluster --name="kops-kube-vpc.dev-sandbox.fpcomplete.com" --zones="us-east-2a" --state "s3://state.kops-kube-vpc.dev-sandbox.fpcomplete.com" --master-size "t2.micro" --node-size "t2.micro" --node-count 2 --vpc "vpc-002bf949cefa58fb5" --target terraform

## clean up terrform and any other files; delete `kops` cluster
clean:
	@kops delete cluster --yes --name=${KOPS_CLUSTER_NAME}
	@terraform destroy
	@rm tf.out terraform.*.backup terraform.tfstate
	@rm -r ./out


## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
