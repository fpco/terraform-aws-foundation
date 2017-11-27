.PHONY: plan apply test destroy clean

.DEFAULT_GOAL = help

## Use Terraform to generate a planfile tf.out
plan:
	@terraform plan -out=tf.out

## Use Terraform to apply the tf.out planfile
apply:
	@terraform apply tf.out

## no-op, fill-in with tests to run
test:
	@echo test

## Use Terraform to destory all resources in the test env
destroy:
	@terraform destroy -force

## rm -rf everything
clean: destroy
	@rm -rf terraform.tfstate terraform.tfstate.backup .terraform
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
