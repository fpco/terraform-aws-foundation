variable "region" {
  description = "The region to put resources in"
  default     = "us-east-1"
}

variable "az" {
  description = "The availability zone to put resources in"
  default     = "us-east-1a"
}

variable "key_name" {
  description = "The keypair used to ssh into the asg intances"
}

variable "key_file" {
  description = "The secret key file to use"
  default     = "~/.ssh/id_rsa"
}

module "setup" {
  source             = "../../tf-modules/nexus-single-node-asg"
  region             = "${var.region}"
  az                 = "${var.az}"
  key_file           = "${var.key_file}"
  key_name           = "${var.key_name}"
  subnet_id          = "${module.vpc.public_subnet_ids[0]}"
  security_group_ids = ["${aws_security_group.nexus-asg.id}"]
  vpc_id             = "${module.vpc.vpc_id}"
}

module "vpc" {
  source              = "../../tf-modules/vpc-scenario-1"
  azs                 = ["${var.az}"]
  name_prefix         = "nexus-single-node-asg"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = "${var.region}"
}

resource "aws_security_group" "nexus-asg" {
  name        = "nexus-asg"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the single-node autoscaling group"

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 8081
    to_port     = 8081
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
