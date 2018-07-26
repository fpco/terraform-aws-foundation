.PHONY: shell render-tls-configs generate-ssh-key generate-tls-certs install-cfssl upload-tls-cert delete-tls-cert render-tfvars network plan apply render-ssh-config init-ebs-volume create-activate-swap terminate-gitlab-server register-gitlab-runner destroy clean help

.DEFAULT_GOAL = help

include vars.mk

PATH:=${PWD}/bin:${PATH}

## Run an interactive shell with all environment variables defined
shell:
	@bash

## Use envsubst to render the json config files for cfssl
render-tls-configs:
	@envsubst <ca-csr.tpl > ca-csr.json
	@envsubst <cert-csr.tpl > cert-csr.json

# Use ssh-keygen to generate a keypair for SSH
generate-ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "test" -f id_rsa
	
## Use cfssl to generate a CA and self-signed server SSL certificate for AWS ELB
generate-tls-certs:
	@cfssl gencert -initca ca-csr.json | cfssljson -bare ca -
	@cfssl gencert -ca=ca.pem -ca-key=ca-key.pem -config=cert-config.json -profile=gitlab cert-csr.json | cfssljson -bare gitlab
	@ls -Alh
	@echo "ca-key.pem is the file to keep very safe"

## use curl to download and install the cfssl executable
install-cfssl:
	@mkdir -p bin
	@curl -s -L -o bin/cfssl https://pkg.cfssl.org/R1.2/cfssl_linux-amd64
	@curl -s -L -o bin/cfssljson https://pkg.cfssl.org/R1.2/cfssljson_linux-amd64
	@chmod +x bin/cfssl bin/cfssljson
	@cfssl version

## Use AWS cli tools to upload the SSL cert to AWS
upload-tls-cert:
	@./upload-gen-cert.sh ${REGION}

## Use AWS cli tools to remove the SSL certs from AWS
delete-tls-cert:
	@./delete-gen-cert.sh ${REGION}

## Use envsubst and project.env to render terraform-tfvars.tpl into terraform.tfvars
## Add ssh key and ssl arn at the same time
render-tfvars:
	@export SSL_ARN=$$(cat upload-gen-cert-arn.txt) && \
	 envsubst <terraform-tfvars.tpl >terraform.tfvars

## Use Terraform to create the VPC and core network resources in a specific order
network:
	@terraform init
	@terraform plan -out=tf.out -target=module.vpc.module.public-subnets
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.vpc.module.public-gateway
	@terraform apply tf.out
	@terraform plan -out=tf.out -target=module.vpc
	@terraform apply tf.out
	@rm -rf tf.out

## Use Terraform to generate a planfile tf.out
plan:
	@terraform plan -out=tf.out

## Use Terraform to apply the tf.out planfile
apply:
	@terraform apply tf.out

## Generate the ssh_config both for Git operations and instance maintenance
render-ssh-config:
	@export SERVER_NAME=$$(terraform output gitlab_server_name) && \
	 export SERVER_IP=$$(./server-ip.sh $$SERVER_NAME $$REGION) && \
	 envsubst <ssh_config.tpl >ssh_config

## use ops cli tool to lookup the public IP of the gitlab instance
##get-ip:
##	@ops aws ec2 asg ips -r $$(terraform output region) $$(terraform output gitlab_asg_name)

## SSH to the gitlab server and use parted and similar tools to setup a partition and filesystem on the EBS volume (run only once per volume)
init-ebs-volume:
	@ssh -F ssh_config data-ops-gitlab "sudo parted --script /dev/xvdf -- mklabel msdos && \
	 sudo parted --script /dev/xvdf -- mkpart primary 0 -1                && \
	 sudo mkfs -t ext4 -F /dev/xvdf1                                      && \
	 sudo e2label /dev/xvdf1 gitlab"

## SSH to the gitlab server and create/activate a swap file for the server
create-activate-swap:
	@ssh -F ssh_config data-ops-gitlab "sudo dd if=/dev/zero of=/var/swap.file bs=1M count=2048 && sudo mkswap /var/swap.file && sudo chmod 600 /var/swap.file && sudo swapon /var/swap.file"

## SSH to the gitlab server and halt the host so it is terminated
terminate-gitlab-server:
	@ssh -F ssh_config data-ops-gitlab "sudo halt & exit"

## SSH to the gitlab server and register the CI runner with the server
register-gitlab-runner:
	@ssh -F ssh_config data-ops-gitlab "sudo gitlab-runner register"

## Use Terraform to destory all resources in the test env
destroy:
	@terraform destroy -force

## rm -rf everything
clean: destroy delete-tls-cert
	@rm -rf terraform.tfstate terraform.tfstate.backup .terraform
	@rm -rf tf.out terraform.tfvars
	@rm -rf id_rsa id_rsa.pub ssh_config
	@rm -rf ca-csr.json cert-csr.json ca.csr upload-gen-cert-arn.txt gitlab.csr *.pem
	@rm -rf bin

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
