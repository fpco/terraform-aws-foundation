# terraform {
#   backend "s3" {
#     bucket = "state.${var.subdomain}"
#   }
# }

variable "subdomain" {
  description = "Subdomain to create the `kops` cluster under."
  type = "string"
}

variable "parent_zone_id" {
  description = "Parent zone ID under which the subdomain is to be created."
  type = "string"
}

variable "vpc_name" {
  default = "kops-kube-vpc"
  description = "VPC ID to use for creating the cluster (if private)."
  type = "string"
}

variable "vpc_cidr" {
  default = "172.100.0.0/16"
  description = "Top-level CIDR for the whole VPC network space"
  type = "string"
}

variable "ssh_pubkey" {
  default = "./id_rsa.pub"
  description = "File path to SSH public key"
  type = "string"
}

variable "ssh_key" {
  default = "./id_rsa"
  description = "File path to SSH public key"
  type = "string"
}

variable "public_subnet_cidrs" {
  default = ["172.100.1.0/24", "172.100.2.0/24", "172.100.3.0/24"]
  description = "Public subnet CIDRs to deploy inside the VPC"
  type = "list"
}


variable "kops_state_bucket_prefix" {
  description = "The prefix to apply to the subdomain to make up the kops state bucket name"
  default = "state"
  type = "string"
}

data "aws_region" "current" {}

data "aws_availability_zones" "available" {}

module "subdomain" {
  source = "../../modules/r53-subdomain"
  name = "${var.subdomain}"
  parent_zone_id = "${var.parent_zone_id}"
}

resource "aws_s3_bucket" "kops_state_bucket" {
  bucket = "${var.kops_state_bucket_prefix}.${module.subdomain.zone_name}"
  acl = "public-read"
  force_destroy = true

  # provisioner "local-exec" {
  #   command = "kops create cluster
  #     --name='${module.subdomain.zone_name}'
  #     --state='s3://${aws_s3_bucket.kops_state_bucket.id}'
  #     --cloud='aws'
  #     --zones='us-east-1c'
  #     --target='terraform'"
  # }
}

# module "vpc" {
#   source = "../../modules/vpc-scenario-1"
#   name_prefix = "${var.vpc_name}"
#   region = "${data.aws_region.current.name}"
#   cidr = "${var.vpc_cidr}"
#   azs = ["${data.aws_availability_zones.available.names[0]}"]
#   public_subnet_cidrs = "${var.public_subnet_cidrs}"
# }

output "subdomain_zone_id" {
  value = "${module.subdomain.zone_id}"
}

output "subdomain_zone_name" {
  value = "${module.subdomain.zone_name}"
}

output "state_bucket_name" {
  value = "${aws_s3_bucket.kops_state_bucket.bucket_domain_name}"
}

output "state_bucket_id" {
  value = "${aws_s3_bucket.kops_state_bucket.id}"
}

output "region" {
  value = "${data.aws_region.current.name}"
}

output "availability_zone" {
  value = "${data.aws_availability_zones.available.names[0]}"
}

# output "vpc_id" {
#   value = "${module.vpc.vpc_id}"
# }
