.PHONY: init ssh-key plan-vpc plan-subnets plan-gateway plan apply destroy clean apply-auto-approve

.DEFAULT_GOAL = help

## Create ssh key
generate-ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "SSH key for lifecycle-hooks-asg example" -f ./id_rsa

## use 'terraform plan' to map out updates to apply
plan:
	@terraform plan -out=tf.out

## use 'terraform apply' to apply updates in a 'tf.out' plan file
apply:
	@terraform apply tf.out

## use 'terraform apply' to apply updates in a 'tf.out' plan file
apply-auto-approve:
	@terraform apply -auto-approve tf.out

## use 'terraform destroy' to remove all resources from AWS
destroy:
	@terraform destroy

## use 'terraform destroy' to remove all resources from AWS
destroy-auto-approve:
	@terraform destroy -auto-approve

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
