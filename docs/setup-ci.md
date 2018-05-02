
This docucment cover the basic structure and steps to setup CI for fpco/terraform-aws-foundation:

* Setup a mirror repository in fpco gitlab in a PULL mode
* Configure ssh to give gitlab permission to pull commits, this will trigger an interval of 1 minute according the Gitlab [docs](https://docs.gitlab.com/ee/workflow/repository_mirroring.html#use-cases)
* Setup environment variables in Gitlab to get access to AWS API to runt terraform
* Add gitlab-ci.yml configuration to run the scripts to check the code:
  * Run TFLint a [Terraform Linter](https://github.com/wata727/tflint) to check the style
  * Run Terraform plan
  * Run Terraform apply but this will expensive to run in AWS. Vagrant it is another less expensive solution but with this in place, will be impossible to test aws networking configurations.
  * The unit test part as part of the continuous integration is the challenging item, this is because there are not many suite test frameworks for aws but I could find a couple that bring this functionality. This will ensure that the vpc, sg, subnets has tagged the correct names.
    - https://github.com/k1LoW/awspec
    - https://github.com/newcontext-oss/kitchen-terraform
  * The previous items, it is not the nicer solution because the test should be written in ruby and using the ruby syntax for `rspec`.
