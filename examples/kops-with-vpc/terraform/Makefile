.PHONY: \
	check-plan-file \
	require-% \
	generate-ssh-keypair \
	init \
	init-plan \
	apply \
	plan \
	plan-destroy \
	clean \
	help

.DEFAULT_GOAL = help

## Make sure we have the required variable set in the environment
require-%:
	@if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## Check tf.out is older than 3 minutes
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`

## Return 1 if tf.out is stale (older than 3 minutes)
check-plan-file:
	@if ! ${STALE_PLAN_FILE} ; then \
		echo "ERROR: Stale tf.out plan file (older than 3 minutes)!"; \
		exit 1; \
	fi

## Generate new ssh keypair
generate-ssh-keypair: require-TF_VAR_cluster_admin_keyname
	@ssh-keygen -t rsa -b 4096 -f ${TF_VAR_cluster_admin_keyname}.pem -C ${TF_VAR_cluster_admin_keyname}

## Runs terraform get and terraform init for env
init:
	@terraform get
	@terraform init


## Generate initial terraform plan with VPC and subnets, among other things
init-plan: require-ENVIRONMENT require-CLUSTER_NAME
	@terraform plan \
		-target="module.vpc" \
		-target="module.public-subnets" \
		-target="aws_key_pair.kops-with-vpc" \
		-target="module.kops-state-bucket" \
		-out=tf.out

## terraform apply
# apply: check-plan-file
apply:
	@terraform apply tf.out

## terraform plan (plan without specifying target modules)
plan:
	@terraform plan -out=tf.out

## Plan the destruction
plan-destroy:
	@terraform plan -destroy -out=tf.out

## Clean up [tf.out, terraform backups, .pem files]
clean:
	@rm -f tf.out terraform.*.backup *.pem*

## Show help.
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
