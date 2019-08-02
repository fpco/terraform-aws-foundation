variable "region" {
  description = "The region to put resources in"
  default     = "us-east-1"
}

variable "az" {
  description = "The availability zone to put resources in"
  default     = "us-east-1c"
}

variable "key_name" {
  description = "The keypair used to ssh into the asg intances"
  default     = "shida-east-1"
}

provider "aws" {
  region = var.region
}

module "vpc" {
  source              = "../../modules/vpc-scenario-1"
  azs                 = [var.az]
  name_prefix         = "bastion-test"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = var.region
  map_on_launch       = false
}

module "bastion" {
  source           = "../../modules/bastion"
  region           = var.region
  key_name         = var.key_name
  public_subnet_id = module.vpc.public_subnet_ids[0]
  identifier       = "test"
  vpc_id           = module.vpc.vpc_id
}
