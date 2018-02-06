.PHONY: render-tls-configs render-tfvars render-ssh-config generate-ssh-key generate-tfvars generate-tls-certs upload-tls install-cfssl network plan apply test destroy

.DEFAULT_GOAL = help

## Use envsubst and vars.env to render the json config files for cfssl
render-tls-configs:
	@. ./vars.env && envsubst <ca-csr.tpl > ca-csr.json
	@. ./vars.env && envsubst <cert-csr.tpl > cert-csr.json

## Use envsubst and project.env to render terraform-tfvars.tpl into terraform.tfvars
render-tfvars:
	@. ./vars.env && envsubst <terraform-tfvars.tpl > terraform.tfvars

## Use envsubst and project.env to render terraform-tfvars.tpl into terraform.tfvars
render-ssh-config:
	@. ./vars.env && envsubst <ssh_config.tpl

## Use ssh-keygen to generate a keypair for SSH
generate-ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "test" -f ./id_rsa

## write out a tfvars file for a test deploy
generate-tfvars:
	@echo "ssh_pubkey = \"./id_rsa.pub\"" > terraform.tfvars
	@echo "region = \"us-east-2\"" >> terraform.tfvars

## Use cfssl to generate a CA and self-signed server SSL certificate for AWS ELB
generate-tls-certs:
	@./cfssl gencert -initca ca-csr.json | cfssljson -bare ca -
	@./cfssl gencert -ca=ca.pem -ca-key=ca-key.pem -config=cert-config.json -profile=gitlab cert-csr.json | cfssljson -bare gitlab
	@ls -Alh
	@echo "ca-key.pem is the file to keep very safe"

## use curl to download and install the cfssl executable
install-cfssl:
	@curl -s -L -o ./cfssl https://pkg.cfssl.org/R1.2/cfssl_linux-amd64
	@curl -s -L -o ./cfssljson https://pkg.cfssl.org/R1.2/cfssljson_linux-amd64
	@chmod +x ./cfssl ./cfssljson
	@./cfssl version

## Use AWS cli tools to upload the SSL cert to AWS
upload-tls-certs:
	@AWS_DEFAULT_REGION=us-east-1 aws acm import-certificate --certificate file://gitlab.pem --private-key file://gitlab-key.pem --certificate-chain file://ca.pem

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

## use ops cli tool to lookup the public IP of the gitlab instance
get-ip:
	@ops aws ec2 asg ips -r $$(terraform output region) $$(terraform output gitlab_asg_name)

## SSH to the gitlab server and use parted and similar tools to setup a partition and filesystem on the EBS volume (run only once per volume)
init-ebs-volume:
	@ssh data-ops-gitlab "sudo parted --script /dev/xvdf -- mklabel msdos && \
	 sudo parted --script /dev/xvdf -- mkpart primary 0 -1                && \
	 sudo mkfs -t ext4 -F /dev/xvdf1                                      && \
	 sudo e2label /dev/xvdf1 gitlab"

## SSH to the gitlab server and create/activate a swap file for the server
create-activate-swap:
	@ssh data-ops-gitlab "sudo dd if=/dev/zero of=/var/swap.file bs=1M count=2048 && sudo mkswap /var/swap.file && sudo chmod 600 /var/swap.file && sudo swapon /var/swap.file"

## SSH to the gitlab server and halt the host so it is terminated
terminate-gitlab-server:
	@ssh data-ops-gitlab "sudo halt"

## SSH to the gitlab server and register the CI runner with the server
register-gitlab-runner:
	@ssh data-ops-gitlab "sudo gitlab-runner register"

## SSH to the gitlab server and cat the runner's config file
cat-gitlab-runner-config:
	@ssh data-ops-gitlab "sudo cat /etc/gitlab-runner/config.toml"

## SSH to the gitlab server and copy the runner's config file to the EBS volume
copy-gitlab-runner-config:
	@ssh data-ops-gitlab "sudo cp /etc/gitlab-runner/config.toml /gitlab/gitlab-runner-config.toml && ls -Alh /gitlab/"

## Login to docker registry
docker-login:
	@docker login $$(terraform registry_url)

## Use 'ops' tool to purge the docker registry's S3 bucket of all objects
purge-docker-registry-storage:
	@ops aws s3 purge $$(terraform output registry_bucket_name) -r $$(terraform output region)

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
