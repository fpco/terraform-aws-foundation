variable "region" {
    default = "us-west-1"
    description = "The AWS region to deploy to"
}
provider "aws" {
  region = "${var.region}"
}
module "vpc" {
  source = "../../tf-modules/packer-vpc"
  region = "${var.region}"
}
// region
output "region" {
  value = "${var.region}"
}
// VPC ID
output "vpc_id" {
  value = "${module.vpc.vpc_id}"
}
// Subnet ID
output "subnet_id" {
  value = "${module.vpc.subnet_id}"
}
// ID of latest trusty AMI
output "trusty_ami_id" {
  value = "${module.vpc.trusty_ami_id}"
}
// ID of latest xenial AMI
output "xenial_ami_id" {
  value = "${module.vpc.xenial_ami_id}"
}
