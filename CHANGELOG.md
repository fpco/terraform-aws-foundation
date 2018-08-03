# v0.8.0

### Summary

* breaking changes to modules that use ASG and previously had `desired_capacity`,
  this parameter is dropped to ensure scaling policies are ok.
* overhaul gitlab examples, see [pr-164][pr-164], [pr-167][pr-167] and [pr-168][pr-168]

### Modules

* `asg`: remove `desired_capacity` input
* `asg`: add `termination_policies` input
* `consul-cluster`: remove `desired_capacity` input
* `consul-leaders`: remove `desired_capacity` input
* `kube-stack`: remove `controller_desired_capacity`  and `worker_desired_capacity` inputs
* `kube-controller-asg`: add new module

### Examples

* refactor the workflow in the `gitlab-asg` example, adding support for TLS and much more
* rename `gitlab-asg` example --> `gitlab-simple-ha`, [pr-164][pr-164]
* duplicate `gitlab-simple-ha` as `gitlab-ha`, both can now get updates independent
  of one another, [pr-167][pr-167]
* update `gitlab-simple-ha` to use EIP instead of ELB, drop TLS, [pr-168][pr-168]

[pr-164]: https://github.com/fpco/terraform-aws-foundation/pull/164
[pr-167]: https://github.com/fpco/terraform-aws-foundation/pull/167
[pr-168]: https://github.com/fpco/terraform-aws-foundation/pull/168


# v0.7.6

### Summary

* enhancements to various modules

### Modules

* `ec2-auto-recover-instance`: fixup interpolation
* `kube-stack`: updates for ELB


# v0.7.5.1

### Summary

* quick bugfix release

### Modules

* `vpc-scenario-2`: drop redundant azs
* `aws-ipsec-vpn`: connection type is hardcoded to 'ipsec.1'


# v0.7.5

### Summary

* enhancements to various modules
* move `terraform-vpc` to examples and update packer build

### Modules

* `r53-subdomain`: added zone name outputs
* `single-node-asg`: drop az from the EBS volume name (it was redundant)
* `aws-ipsec-vpn`: add support for govcloud and FIPS endpoints
* `init-snippet-curator`:
  * bugfix for `master_only`
  * parametize index retention units
  * pin pip at 9.x

### Examples

* move `terraform-vpc` to `examples`


# v0.7.4

### Summary

* Refine test suite and get CI build green
* Improve type checking in module variables
* Demo how to validate S3 policies with new env and test suite
* Various module bugfixes

### Examples

* `s3-full-access-policy`: add env and initial test suite - [#122][122]
* `vpc-scenario-2`: refactor hardcoded AZ list, use locals - [#134][132]

### Modules

* `prometheus-server`: sync with changes to variables/modules - [#107][107]
* `credstash-setup`: pip 10 is a failure, use pip 9.x for now - [#145][145]
* `init-snippet-install-awscli`: pip 10 is a failure, use pip 9.x for now - [#145][145]
* `bind-server`: allow `db_records_folder` variable to be empty - [#135][135]
* add type definitions to all variables in all modules - [#108][108]

### Packer

* `kubespray`: parametize docker tag - [#123][123]

### Tests

* add `Makefile` to install tools and init Terraform
* use `tflint` 0.6.0 to resolve [#107][107]
* drop `vpc-legacy` module from the test suite
* enable tfinit for [#104][104], disable `undeclared-variables` for [#141][141]

[122]: https://github.com/fpco/terraform-aws-foundation/issues/122
[132]: https://github.com/fpco/terraform-aws-foundation/issues/132
[107]: https://github.com/fpco/terraform-aws-foundation/issues/107
[145]: https://github.com/fpco/terraform-aws-foundation/pull/145
[108]: https://github.com/fpco/terraform-aws-foundation/issues/108
[135]: https://github.com/fpco/terraform-aws-foundation/issues/135
[123]: https://github.com/fpco/terraform-aws-foundation/issues/123
[141]: https://github.com/fpco/terraform-aws-foundation/issues/141


# v0.7.3

### Examples

#### `cloud-dev-workspace`

* correct `vpc_cidr`

#### `kube-stack-private`

* fixup helpdoc in Makefile, #76
* refactor out hardcoded AZ list, use `locals`
* add `generate-ssh-key` target to Makefile
* `make network` should target `nat-gateways`

#### `vpc-scenario-1`

* az bugfix for web instance, #125
* update for consistency with other example env

#### `vpc-scenario-2`

* fixup README
* fixup `make test`
* add missing security group rule for ELB
* update for consistency with other example env

#### `vpc-scenario-2-nat-instance`

* fix build, improve Makefile

#### `vpc-scenario-2-nat-instance-per-az`

* fix build, improve Makefile


### Modules

#### `nat-gateways`:

* use `aws_subnet` data source to lookup subnets
* use `element()` instead of `var.foo[]` syntax

#### `bind-server`

* support empty private_ips, allow disabling DNS
* Initial CI setup, run `tflint`


### Documentation

* how to setup ci, #113


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
