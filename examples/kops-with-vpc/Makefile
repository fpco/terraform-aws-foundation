.PHONY: \
	require-% \
	copy-keys \
	copy-vars \
	copy

.DEFAULT_GOAL = help

## Make sure we have the required variable set in the environment
require-%:
	@if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## Copy generated SSH keys from the terraform subfolder to kubernetes/$ENVIRONMENT
copy-keys:
	@cp terraform/*.pem* kubernetes/${ENVIRONMENT}

## Replaces `terraform output $OUT` as $AS into kubernetes/$ENVIRONMENT/env. Ex: `make copy OUT=vpc_id AS=VPC_ID`
copy:
	@cd terraform; sed -i "/export ${AS}/c\export ${AS}=$$(terraform output ${OUT} | tr -d '\n')" ../kubernetes/${ENVIRONMENT}/env; cd ..;

copy-vars:
	$(MAKE) -s copy OUT=region AS=REGION
	$(MAKE) -s copy OUT=vpc_id AS=VPC_ID
	$(MAKE) -s copy OUT=vpc_cidr AS=VPC_CIDR
	$(MAKE) -s copy OUT=subnet_ids AS=KOPS_SUBNET_IDS
	$(MAKE) -s copy OUT=subnet_cidrs AS=KOPS_SUBNET_CIDRS
	$(MAKE) -s copy OUT=subnet_azs AS=KOPS_ZONES
	$(MAKE) -s copy OUT=name_prefix AS=ENVIRONMENT
	$(MAKE) -s copy OUT=kubernetes_cluster_name AS=CLUSTER_NAME
	$(MAKE) -s copy OUT=kops_state_bucket AS=KOPS_BUCKET

## Run the clean target in the terraform directory
clean-terraform:
	$(MAKE) -s -C terraform clean

## Run the clean target in the kubernetes directory
clean-kubernetes:
	$(MAKE) -s -C kubernetes clean

## Run the clean target on subdirectories
clean: clean-terraform clean-kubernetes
	@:

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
