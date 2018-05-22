.PHONY: install-tools init plan apply destroy clean

.DEFAULT_GOAL = help

# Hardcoding value of 3 minutes when we check if the plan file is stale
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`

## Check if tf.out is stale (Older than 2 minutes)
check-plan-file:
	@if ! ${STALE_PLAN_FILE} ; then \
		echo "ERROR: Stale tf.out plan file (older than 3 minutes)!"; \
		exit 1; \
	fi

## install the various tools needed
install-tools:
	@wget https://github.com/wata727/tflint/releases/download/v0.6.0/tflint_linux_amd64.zip && unzip tflint_linux_amd64.zip && mv tflint ~/bin/

## Runs terraform get and terraform init for env
init:
	@terraform init

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

## terraform destroy everything
destroy:
	@terraform destroy

## rm -rf all files and state
clean:
	@rm -f tf.out
	@rm -f terraform.tfvars
	@rm -f terraform.*.backup
	@rm -f terraform.tfstate

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
