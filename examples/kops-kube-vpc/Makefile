# Hardcoding value of 3 minutes when we check if the plan file is stale
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`
SHELL := /bin/bash

AWS_AVAILABILITY_ZONE := `terraform output -json | jq '.availability_zone.value' | tr -d '"'`
KOPS_STATE_BUCKET := `terraform output -json | jq '.state_bucket_id.value' | tr -d '"'`
KOPS_CLUSTER_NAME := `terraform output -json | jq '.subdomain_zone_name.value' | tr -d '"'`
KOPS_CLUSTER_ZONE := `terraform output -json | jq '.subdomain_zone_id.value' | tr -d '"'`
KOPS_AWS_VPC := `terraform output -json | jq '.vpc_id.value' | tr -d '"'`

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

## Prepare the environment with a subdomain and vpc for kops
prepare-environment: plan apply

## Generate terraform plan using `kops`
generate-kops-plan:
	@echo "name:" ${KOPS_CLUSTER_NAME} "| zone:" ${AWS_AVAILABILITY_ZONE} "| state_store:" "s3://${KOPS_STATE_BUCKET}" "| vpc:" ${KOPS_AWS_VPC}
	@echo "Verify, then press ENTER to continue..."
	@read _input
	@kops create cluster \
		--name=${KOPS_CLUSTER_NAME} \
		--dns-zone=${KOPS_CLUSTER_ZONE} \
		--zones=${AWS_AVAILABILITY_ZONE} \
		--master-size="t2.micro" \
		--node-size="t2.micro" \
		--node-count 2 \
		--vpc ${KOPS_AWS_VPC} \
		--state "s3://${KOPS_STATE_BUCKET}" \
		--target=terraform

edit-kops-plan:
	@kops edit cluster \
		--name=${KOPS_CLUSTER_NAME} \
		--state "s3://${KOPS_STATE_BUCKET}"

apply-kops-plan:
	@pushd out/terraform && terraform apply && popd;

## Clean up terrform and other files; also delete the `kops` cluster
clean:
	@kops delete cluster \
		--name=${KOPS_CLUSTER_NAME} \
		--state "s3://${KOPS_STATE_BUCKET}"
	@pushd out/terraform && terraform destroy && popd;
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
