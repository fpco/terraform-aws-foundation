.PHONY: plan apply help clean generate-ssh-keypair check-env \
	init check-plan-file

.DEFAULT_GOAL = help

# Hardcoding value of 3 minutes when we check if the plan file is stale
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`

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

## initial terraform plan (makes VPC, subnets, etc)
init-plan:
	@terraform plan \
		-target="module.vpc1" \
		-target="module.vpc2" \
		-target="module.vpc1-public-subnets" \
		-target="module.vpc2-public-subnets" \
		-target="module.vpc1-sg" \
		-target="module.vpc2-sg" \
		-target="module.vpc1-open-ssh" \
		-target="module.vpc1-open-egress" \
		-target="module.vpc2-open-ssh" \
		-target="module.vpc2-open-egress" \
		-target="module.ubuntu-xenial-ami" \
		-out=tf.out

## Provision gateway (Subnets all need to exists first)
gateway-plan:
	@terraform plan \
		-target="module.vpc1-public-gateway" \
		-target="module.vpc2-public-gateway" \
		-out=tf.out

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform destroy everything
destroy-plan:
	@terraform plan -destroy -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

## Cleans current dir from uneeded files
clean:
	@rm -f tf.out
	@rm -f id_rsa
	@rm -f id_rsa.pub
	@rm -f terraform.tfvars
	@rm -f terraform.*.backup
	@rm -f terraform.tfstate

## Generate new ssh keypair.
generate-ssh-keypair:
	@ssh-keygen -t rsa -b 4096 -f id_rsa -C "Test project vpc peering scenario admin"

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

