.PHONY: ssh-key init plan-subnets plan-gateways plan apply test destroy clean

.DEFAULT_GOAL = help

# Hardcoding value of 3 minutes when we check if the plan file is stale
STALE_PLAN_FILE := `find "tf.out" -mmin -3 | grep -q tf.out`

## Check if tf.out is stale (Older than 2 minutes)
check-plan-file:
	@if ! ${STALE_PLAN_FILE} ; then \
		echo "ERROR: Stale tf.out plan file (older than 3 minutes)!"; \
		exit 1; \
	fi

## Create ssh key
ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "SSH key for vpc-scenario-3 example" -f ./id_rsa

## Create ssh key for openvpn server
ssh-key-remote-vpc:
	@cd remote-environment/ && make ssh-key

## Runs terraform get and terraform init for the remote vpc weher the VPN installed
init-remote-vpc:
	@cd remote-environment/ && make init && make subnets
	@cd remote-environment/ && make plan && make apply

## Runs terraform get and terraform init for env
init:
	@terraform get
	@terraform init

## use 'terraform plan' to 'target' the vpc module
vpc:
	@terraform plan -out tf.out -target=module.vpc.module.vpc
	@terraform apply tf.out

## use 'terraform plan' to 'target' the public/private subnets in the vpc module
subnets:
	@terraform plan \
		-target="module.vpc.module.public-subnets" \
		-target="module.vpc.module.private-subnets" \
		-out=tf.out
	@terraform apply tf.out

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

# Setup IPSec in the openvpn server
setup-ipsec:
	#@terraform destroy \
	#	-target="module.vpc.module.vpn.aws_vpn_connection.main"
	#@terraform plan  \
	#	-target="module.vpc.module.vpn" \
	#  -out=tf.out
	#@terraform apply tf.out
	@scripts/set-ipsec-configurations.sh
	@scripts/upload-ipsec-configs.sh
	@scripts/restart-ipsec-service.sh
	@rm remote-environment/ipsec*
	@echo "Download openvpn client config from this URL https://$$(terraform output -state='remote-environment/terraform.tfstate' openvpn-public-eip):943"

restart-ipsec:
	@scripts/restart-ipsec-service.sh

## use curl to hit the ELB to confirm the VPC came online ok
test:
	@curl -v http://$$(terraform output elb_dns)/

## terraform destroy everything
destroy:
	@terraform destroy

## rm -rf all files and state
clean:
	@rm -f tf.out
	@rm -f id_rsa
	@rm -f id_rsa.pub
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
