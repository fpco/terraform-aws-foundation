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
  name_prefix         = "eiptest"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = var.region
  map_on_launch       = false
}

module "snasg" {
  source             = "../../modules/single-node-asg"
  name_prefix        = "unit"
  name_suffix        = "eiptest"
  ami                = module.ubuntu-ami.id
  instance_type      = "t2.micro"
  region             = var.region
  key_name           = var.key_name
  subnet_id          = module.vpc.public_subnet_ids[0]
  security_group_ids = [aws_security_group.eiptest.id]
  assign_eip         = false # true case is tested in bastion-test example
}

module "ubuntu-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "16.04"
}

resource "aws_security_group" "eiptest" {
  name   = "eiptest"
  vpc_id = module.vpc.vpc_id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}
