variable "region" {
  default     = "us-west-2"
  description = "AWS region this env is deployed to"
}

variable "name" {
  default     = "kube-stack-test"
  description = "name of this deployment, the VPC and all resources therein"
}

variable "instance_type" {
  default     = {
    "bastion"         = "t2.nano"
    "kube_worker"     = "t2.medium"
    "kube_controller" = "t2.medium"
  }
  description = "map of instance types to use"
}

#variable "kube_cluster_name" {
#  description = "name of the kube cluster deployed to the VPC, used to tag resources"
#}

variable "ssh_pubkey" {
  default     = "~/.ssh/id_rsa.pub"
  description = "file path to the pubkey to use for SSH and AWS keypair"
}

variable "ssh_key" {
  default     = "~/.ssh/id_rsa"
  description = "file path to the private key to use for SSH provisioner"
}

# VPC Network

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.10.0.0/16"
}

variable "public_subnet_cidrs" {
  description = "list of CIDR ranges for the public subnets"
  default     = ["10.10.10.0/24", "10.10.11.0/24"] #, "10.120.12.0/24"]
}

variable "private_subnet_cidrs" {
  description = "list of CIDR ranges for the private subnets"
  default     = ["10.10.20.0/24", "10.10.21.0/24"] #, "10.120.22.0/24"]
}

variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "canonical_account_id" {
  description = "AWS  account it for Canonical, used to lookup Ubuntu AMIs"
  default     = "099720109477"
  # for GovCloud, change to "513442679011"
}

variable "coreos_account_id" {
  description = "AWS account it for CoreOS, used to lookup CoreOS AMIs"
  default     = "595879546273"
  # for GovCloud, change to "190570271432"
}
