.PHONY: plan apply help clean generate-ssh-keypair import-ssh-keypair \
	init check-plan-file

.DEFAULT_GOAL = help

export PASSWORD_STORE_DIR ?= ${PWD}/../password-store

# Hardcoding value of 3 minutes when we check if the plan file is stale
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`

require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

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

## EXAMPLE: initial terraform plan (makes VPC, subnets, etc)
#init-plan: require-ENVIRONMENT require-CLUSTER_NAME
#	@terraform plan \
#		-target="module.vpc" \
#		-target="module.public-subnets" \
#		-target="module.open-ssh" \
#		-target="module.open-egress" \
#		-target="module.open-egress" \
#		-out=tf.out

init-plan: require-ENVIRONMENT require-CLUSTER_NAME
	@terraform plan \
		-target="module.vpc" \
		-target="module.public-subnets" \
		-out=tf.out

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

plan-destroy:
	@terraform plan -destroy -out=tf.out

## Cleans current dir from uneeded files
clean:
	@rm -f tf.out
	@rm -f terraform.*.backup
	@rm -f ../**/*.pem*

## Generate new ssh keypair.
generate-ssh-keypair: require-TF_VAR_cluster_admin_keyname
	@ssh-keygen -t rsa -b 4096 -f ${TF_VAR_cluster_admin_keyname}.pem -C ${TF_VAR_cluster_admin_keyname}

## Import keypair into aws
import-ssh-keypair: require-CLUSTER_NAME
	@aws ec2 import-key-pair --key-name ${TF_VAR_cluster_admin_keyname} --region ${TF_VAR_aws_region} --public-key-material "`cat ${TF_VAR_cluster_admin_keyname}.pem.pub)`"

## Delete keypair from AWS
delete-ssh-keypair: require-TF_VAR_cluster_admin_keyname
	@aws ec2 delete-key-pair --key-name ${TF_VAR_cluster_admin_keyname} --region ${TF_VAR_aws_region}

## Saves admin keys to password store
save-ssh-keys: require-CLUSTER_NAME require-PASSWORD_STORE_DIR
	@pass insert -f -m clusters/${CLUSTER_NAME}/keys/admin.pem < ${TF_VAR_cluster_admin_keyname}.pem
	@pass insert -f -m clusters/${CLUSTER_NAME}/keys/admin.pem.pub < ${TF_VAR_cluster_admin_keyname}.pem.pub

## Saves terraform.tfvars to password store
save-vars: require-CLUSTER_NAME require-PASSWORD_STORE_DIR
	@pass insert -f -m tfvars/${CLUSTER_NAME}.tfvars < terraform.tfvars

## Fetch terraform.tfvars from password store
fetch-vars: require-CLUSTER_NAME require-PASSWORD_STORE_DIR
	@pass tfvars/${CLUSTER_NAME}.tfvars > terraform.tfvars

## Fetch ssh public and private key from pw store
fetch-ssh-keys: require-CLUSTER_NAME require-PASSWORD_STORE_DIR
	@pass clusters/${CLUSTER_NAME}/keys/admin.pem > ${TF_VAR_cluster_admin_keyname}.pem
	@pass clusters/${CLUSTER_NAME}/keys/admin.pem > ${TF_VAR_cluster_admin_keyname}.pem.pub

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
