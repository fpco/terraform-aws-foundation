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

ssh-key:
	@ssh-keygen -q -N "" -b 4096 -C "SSH key for vpc-scenario-1 example" -f ./id_rsa

plan-vpc:
	@terraform plan \
    -target="module.vpc.module.vpc" \
    -out=tf.out

plan-subnets:
	@terraform plan \
    -target="module.vpc.module.public-subnets" \
    -out=tf.out

plan-gateway:
	@terraform plan \
    -target="module.vpc.module.public-gateway" \
    -out=tf.out

plan:
	@terraform plan -out=tf.out

apply: check-plan-file
	@terraform apply tf.out

destroy:
	@terraform destroy

clean:
	@rm -f tf.out
	@rm -f id_rsa
	@rm -f id_rsa.pub
	@rm -f terraform.tfvars
	@rm -f terraform.*.backup
	@rm -f terraform.tfstate
