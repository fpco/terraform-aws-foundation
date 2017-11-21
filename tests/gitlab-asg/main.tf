variable "name" {
  default = "gitlab-asg-test"
}

variable "region" {
  default = "us-east-1"
}

variable "key_file" {
  description = "The secret key file to use"
}

data "aws_availability_zones" "available" {}

module "ubuntu-xenial-ami" {
  source  = "../../tf-modules/ami-ubuntu"
  release = "16.04"
} 

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.key_file)}"
}

module "setup" {
  name_prefix        = "${var.name}"
  source             = "../../tf-modules/gitlab-single-node-asg"
  region             = "${var.region}"
  az                 = "${element(data.aws_availability_zones.available.names, 0)}"
  key_file           = "${var.key_file}"
  key_name           = "${aws_key_pair.main.key_name}"
  instance_ami       = "${module.ubuntu-xenial-ami.id}"
  subnet_id          = "${module.vpc.public_subnet_ids[0]}"
  vpc_id             = "${module.vpc.vpc_id}"
  security_group_ids = ["${aws_security_group.gitlab.id}"]
}

module "vpc" {
  source              = "../../tf-modules/vpc-scenario-1"
  azs                 = ["${slice(data.aws_availability_zones.available.names, 0, 1)}"]
  name_prefix         = "${var.name}"
  cidr                = "192.168.0.0/16"
  public_subnet_cidrs = ["192.168.0.0/16"]
  region              = "${var.region}"
}

resource "aws_security_group" "gitlab" {
  name        = "gitlab-asg"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the single-node autoscaling group"
}

module "ssh-rule" {
  source            = "../../tf-modules/ssh-sg"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "http-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 80
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "https-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 443
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "gitlab-ssh-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 8022
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.gitlab.id}"
}

module "open-egress-rule" {
  source            = "../../tf-modules/open-egress-sg"
  security_group_id = "${aws_security_group.gitlab.id}"
}

}
