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
	@export AWS_AVAILABILITY_ZONE=$(terraform output -json | jq '.availability_zone.value' | tr -d '"')
	@export KOPS_STATE_STORE=s3://$(terraform output -json | jq '.state_bucket_id.value' | tr -d '"')
	@export KOPS_CLUSTER_NAME=$(terraform output -json | jq '.subdomain_zone_name.value' | tr -d '"')
	@export KOPS_AWS_VPC=$(terraform output -json | jq '.vpc_id.value' | tr -d '"')
	@echo "Done exporting..."
	@echo "VPC ID:" ${KOPS_AWS_VPC}
	@echo "Press ENTER to continue"
	@read _input
	@kops create cluster \
		--name=${KOPS_CLUSTER_NAME} \
		--cloud=aws \
		--zones=${AWS_AVAILABILITY_ZONE} \
		--master-size="t2.micro" \
		--node-size="t2.micro" \
		--node-count=2 \
		--vpc="vpc-b61951d3" \
		--target=terraform \
		${KOPS_CLUSTER_NAME}

##

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
