variable "region" {
  default     = "us-west-1"
  description = "The AWS region to deploy to"
}

provider "aws" {
  region = var.region
}

module "vpc" {
  source = "../../modules/packer-vpc"
  region = var.region
}

output "region" {
  value       = var.region
  description = "region"
}

output "vpc_id" {
  value       = module.vpc.vpc_id
  description = "VPC ID"
}

output "subnet_id" {
  value       = module.vpc.subnet_id
  description = "Subnet ID"
}

output "trusty_ami_id" {
  value       = module.vpc.trusty_ami_id
  description = "ID of latest trusty AMI"
}

output "xenial_ami_id" {
  value       = module.vpc.xenial_ami_id
  description = "ID of latest xenial AMI"
}

