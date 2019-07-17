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
	@ssh-keygen -q -N "" -b 4096 -C "SSH key for vpc-scenario-2 example" -f ./id_rsa

## Runs terraform get and terraform init for env
init:
	@terraform get
	@terraform init

## use 'terraform plan' to 'target' the public/private subnets in the vpc module
plan-subnets:
	@terraform plan \
		-target="module.vpc.module.public-subnets" \
		-target="module.vpc.module.private-subnets" \
		-out=tf.out

## use 'terraform plan' to 'target' the public and NAT gateways in the vpc module
plan-gateways:
	@terraform plan \
		-target="module.vpc.module.public-gateway" \
		-target="module.vpc.module.nat-gateway" \
		-out=tf.out

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

## use curl to hit the ELB to confirm the VPC came online ok
test:
	@curl -v http://$$(terraform output elb_dns)/

## Use ops http polling to block until ELB is available, should happen in 5 minutes or less
poll:
	echo "will now poll that URL until we see 200 (and timeout after 5 minutes)"
	ops http poll --interval 5 --retry 60 http://$(shell terraform output elb_dns)/

## use existing targets to run it all (hopefully)
all: ssh-key init plan-subnets apply plan-gateways apply plan apply poll

## Use ops to lookup IPs of the EC2 nodes in the ASG
lookup-ips:
	ops aws ec2 asg ips --region $(shell terraform output region) --private $(shell terraform output asg_name)

## Use ops to terminate all of the EC2 nodes in the ASG
terminate-ec2-nodes:
	ops aws ec2 asg terminate --region $(shell terraform output region) $(shell terraform output asg_name)


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
