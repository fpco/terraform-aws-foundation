.PHONY: init ssh-key plan-vpc plan-subnets plan-gateway plan apply destroy clean

.DEFAULT_GOAL = help

## Create ssh key
generate-ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "SSH key for vpc-scenario-1 example" -f ./id_rsa

## Create network
network:
	@terraform init
	@terraform plan -out=tf.out -target=module.vpc.module.public-subnets
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.vpc.module.public-gateway
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.vpc
	@terraform apply tf.out
	@rm -rf tf.out

## use 'terraform plan' to map out updates to apply
plan:
	@terraform plan -out=tf.out

## use 'terraform apply' to apply updates in a 'tf.out' plan file
apply:
	@terraform apply tf.out

## Generate the ssh_config both for Git operations and instance maintenance
render-ssh-config:
	@./server-ip.sh

## CPU stress stress (first render ssh instances, as they might have changed from ASGload scaling)
generate-cpu-stress:
	@./server-ip.sh
	@./cpu-stress.sh

## Memory stress stress (first render ssh instances, as they might have changed from ASGload scaling)
generate-mem-stress:
	@./server-ip.sh
	@./mem-stress.sh

## use 'terraform destroy' to remove all resources from AWS
destroy:
	@terraform destroy

## rm -rf all files and state
clean: destroy
	@rm -f tf.out
	@rm -f id_rsa
	@rm -f id_rsa.pub
	@rm -f terraform.*.backup
	@rm -f terraform.tfstate
	@rm -f ssh_config
	@rm -rf .terraform

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
