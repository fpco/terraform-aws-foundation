# v0.7.3

* `bind-server`: support empty private_ips, allow disabling DNS
* Initial CI setup, run `tflint`
* `ex/vpc-scenario-1`: az bugfix for web instance, #125
* update VPC Scenarios 1 and 2 for consistency with other example env
* `docs`: how to setup ci, #113


# v0.7.2

* `ex/kube-stack-private`: tag public subnets for ELBs
* `ex/kube-stack-private`: use `extra_tags` for changes in Kubernetes
* `kube-stack`: add missing tag for kubernetes
* `vpc-scenario-2`: refactor how extra_tags are used
* `vpc-scenario-2`: use `var.private_subnet_cidrs` for `nat_count`
* `vpc-scenario-4`: fixup outputs and subnet module parameters
* `examples/nexus-asg`: Use the `ubuntu-ami` module, drop hardcoded AMI
* `docs`: add `testing-design` doc
* `examples/vpc-scenario-peering`: Correct destroy target in Makefile, this was
  previously unable to properly destroy the deployment.
* `modules/vpc-scenario-2`: use `var.private_subnet_cidrs` for `nat_count`
* `vpc-scenario-2`: refactor how `extra_tags` are used. Support adding specific
  tags to specific components in the boxed VPC. Update the `kube-stack-private`
  env to use these new variables. This greatly simplifies how tags are supported
  and used in the kubernetes env.
* Refactor inline IAM policies into proper data sources
* Implement lightweight test framework to automate finding bugs in our modules
  and example Terraform env


# v0.7.1

Minor bugfix release

* Fixup quotes in `init-snippet-exec` module


# v0.7.0

Massive update to nearly all aspects of the module repo, including:

* New modules and example environments
* Refactored security group modules
* Updates to get modules visible on the Terraform registry
* Many updates to various modules
