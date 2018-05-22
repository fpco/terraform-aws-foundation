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

## terraform plan (makes everything)
plan:
	@terraform plan -out=tf.out

## terraform output - send module output to config file
output:
	@terraform output -json > config.json

## terraform apply
apply: check-plan-file
	@terraform apply tf.out

## build and run the test executable
test:
	@cd s3-full-access-policy-tests && \
			stack build && \
			stack exec s3-full-access-policy-tests-exe

# clean up terrform and any other files
clean:
	@terraform destroy
	@rm -f tf.out
	@rm -f terraform.tfvars
	@rm -f terraform.*.backup
	@rm -f terraform.tfstate
