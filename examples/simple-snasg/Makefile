MAKEFLAGS = -s

.PHONY: ssh-key init plan apply destroy clean

.DEFAULT_GOAL = help

export TERRAFORM = terraform

export AWS_CONFIG_FILE = ${HOME}/.aws/config
export AWS_SHARED_CREDENTIALS_FILE = ${HOME}/.aws/credentials
export AWS_ENV = AWS_CONFIG_FILE=$(AWS_CONFIG_FILE) AWS_SHARED_CREDENTIALS_FILE=$(AWS_SHARED_CREDENTIALS_FILE) aws-env

require-%:
	if [ "${${*}}" = "" ]; then \
	        echo "ERROR: Environment variable not set: \"$*\""; \
	        exit 1; \
	fi

PLAN_FILE=tf.out

## Check if tf.out is stale (Older than 2 minutes)
check-plan-file:
	@if ! ${STALE_PLAN_FILE} ; then \
		echo "ERROR: Stale tf.out plan file (older than 3 minutes)!"; \
		exit 1; \
	fi

## Create an ssh key
ssh-key:
	ssh-keygen -q -N "" -b 4096 -C "SSH key for the SNASG foundation demo" -f ./id_rsa

## Runs terraform get and terraform init for env
init: require-TERRAFORM
	$(AWS_ENV) $(TERRAFORM) get
	$(AWS_ENV) $(TERRAFORM) init

## terraform plan
plan: require-TERRAFORM require-PLAN_FILE
	$(AWS_ENV) $(TERRAFORM) plan -out=${PLAN_FILE}

## terraform apply
apply: require-TERRAFORM require-PLAN_FILE check-plan-file
	$(AWS_ENV) $(TERRAFORM) apply -parallelism=7 ${PLAN_FILE}

destroy: require-TERRAFORM require-PLAN_FILE check-plan-file
	$(AWS_ENV) $(TERRAFORM) destroy

## rm -rf all files and state
clean: destroy
	rm -f tf.out
	rm -f id_rsa
	rm -f id_rsa.pub
	rm -f terraform.tfvars
	rm -f terraform.tfstate

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
