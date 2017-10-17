variable "region" {
  default = "us-east-1"
}

variable "az" {
  default = "us-east-1a"
}

variable "key_name" {
  description = "Keypair name to log into the ec2 instance"
}

variable "key_file" {
  description = "The secret key file to use"
  default     = "~/.ssh/id_rsa"
}

module "setup" {
  name               = "test"
  source             = "../../tf-modules/gitlab-single-node-asg"
  region             = "${var.region}"
  az                 = "${var.az}"
  key_file           = "${var.key_file}"
  key_name           = "${var.key_name}"
  subnet_id          = "${module.vpc.public_subnet_ids[0]}"
  vpc_id             = "${module.vpc.vpc_id}"
  security_group_ids = ["${aws_security_group.gitlab-asg.id}"]
}

module "vpc" {
  source              = "../../tf-modules/vpc-scenario-1"
  azs                 = ["${var.az}"]
  name_prefix         = "gitlab-single-node-asg"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = "${var.region}"
}

resource "aws_security_group" "gitlab-asg" {
  name        = "gitlab-asg"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the single-node autoscaling group"

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 8022
    to_port     = 8022
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
