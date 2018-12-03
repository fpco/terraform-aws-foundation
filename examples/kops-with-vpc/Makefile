.PHONY: \
	require-% \
	copy-keys \
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

## Run the clean target in the terraform directory
clean-terraform:
	@$(MAKE) -C terraform clean

## Run the clean target in the kubernetes directory
clean-kubernetes:
	@$(MAKE) -C kubernetes clean

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
