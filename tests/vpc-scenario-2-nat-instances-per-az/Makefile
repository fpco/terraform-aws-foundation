.PHONY: generate-ssh-key generate-tfvars network plan apply test get-ip destroy clean

.DEFAULT_GOAL = help

## Use ssh-keygen to generate a keypair for SSH
generate-ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "test" -f ./id_rsa

## write out a tfvars file for a test deploy
generate-tfvars:
	@echo "ssh_pubkey = \"./id_rsa.pub\""  > terraform.tfvars
	@echo "ssh_key = \"./id_rsa\""        >> terraform.tfvars
	@echo "region = \"us-east-2\""        >> terraform.tfvars

## Use Terraform to create the VPC and core network resources in a specific order
network:
	@terraform init
	@terraform plan -out=tf.out -target=module.public-subnets
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.private-subnets
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.public-gateway
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.nat-instance
	@terraform apply tf.out
	@rm -rf tf.out

## Use Terraform to generate a planfile tf.out
plan:
	@terraform plan -out=tf.out

## Use Terraform to apply the tf.out planfile
apply:
	@terraform apply tf.out

## Use curl to ping the ELB as a test
test:
	@curl -v http://$$(terraform output elb_dns)

## use ops cli tool to lookup the public IP of the gitlab instance
get-ip:
	@ops aws ec2 asg ips -r $$(terraform output region) --private $$(terraform output web_asg_name)

## Use Terraform to destory all resources in the test env
destroy:
	@terraform destroy -force

## rm -rf everything
clean: destroy
	@rm -rf terraform.tfstate terraform.tfstate.backup .terraform
	@rm -rf id_rsa id_rsa.pub
	@rm -rf tf.out terraform.tfvars

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
