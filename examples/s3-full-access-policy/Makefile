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

buckets-plan:
	@terraform plan \
		-target="module.test-bucket" \
		-out=tf.out

policy-plan:
	@terraform plan \
		-target="module.s3-full-access-policy" \
		-out=tf.out

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

# test:

clean:
	@terraform destroy
	@rm -f tf.out
	@rm -f terraform.tfvars
	@rm -f terraform.*.backup
	@rm -f terraform.tfstate
