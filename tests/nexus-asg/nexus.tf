/**
 * Run a NEXUS RM on a single EC2 instance.
 * This instance will be part of a single-node autoscaling group
 * that shares an EBS volume to store data.
 *
 * Note that there is a peculiarity with the EBS volume in that it
 * requires some manual setup the very first time to make it available
 * for use (unless a snapshot id is supplied):
 *
 * parted --script /dev/xvdf -- mklabel msdos
 * parted --script /dev/xvdf -- mkpart primary 0 -1
 * mkfs -t ext4 -F /dev/xvdf1
 * e2label /dev/xvdf1 nexus
 *
 */

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

module "vpc" {
  source              = "../../tf-modules/vpc-scenario-1"
  azs                 = ["${var.az}"]
  name_prefix         = "nexus-single-node-asg"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = "${var.region}"
}

module "snasg" {
  source             = "../../tf-modules/single-node-asg"
  name               = "test"
  name_suffix        = "nexus-asg"
  ami                = "ami-cd0f5cb6"
  instance_type      = "t2.micro"
  region             = "${var.region}"
  az                 = "${var.az}"
  key_file           = "${var.key_file}"
  key_name           = "${var.key_name}"
  subnet_id          = "${module.vpc.public_subnet_ids[0]}"
  security_group_ids = ["${aws_security_group.nexus-asg.id}"]

  init_prefix = <<END_INIT
apt-get update
${module.init-install-awscli.init_snippet}
${module.init-install-ops.init_snippet}
END_INIT

  init_suffix = <<END_INIT
${module.init-nexus.init_snippet}
END_INIT
}

module "init-nexus" {
  source = "../../tf-modules/init-snippet-nexus"
}

module "init-install-awscli" {
  source = "../../tf-modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../tf-modules/init-snippet-install-ops"
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
