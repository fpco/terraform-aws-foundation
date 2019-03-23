/**
 * ## Example to demonstrate basic ASG integration with lifecycle hooks
 */
provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

data "aws_region" "current" {}

# Cloud init script for the autoscaling group
data "template_file" "main" {
  template = "${file("${path.module}/cloud-config.yml")}"

  vars {
    region          = "${data.aws_region.current.name}"
    stack_name      = "${var.lifecycle_name_prefix}-asg"
    lifecycle_topic = "${aws_sns_topic.main.arn}"
    elb_name        = "${aws_elb.web.name}"
  }
}

module "vpc" {
  source      = "../../modules/vpc-scenario-1"
  name_prefix = "${var.name}"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  azs         = ["${local.azs}"]

  extra_tags = {
    kali = "ma"
  }

  public_subnet_cidrs = ["${var.public_subnet_cidrs}"]
}

# Use the latest Amazon Linux 2 AMI
data "aws_ami" "linux2" {
  owners      = ["amazon"]
  most_recent = true

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  filter {
    name   = "architecture"
    values = ["x86_64"]
  }

  filter {
    name   = "root-device-type"
    values = ["ebs"]
  }

  filter {
    name   = "name"
    values = ["amzn2-ami*gp2"]
  }
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

# Security group for the elastic load balancer, web instance, only accessible from ELB
module "elb-sg" {
  source      = "../../modules/security-group-base"
  description = "Allow public access to ELB in ${var.name}"
  name        = "${var.name}-elb"
  vpc_id      = "${module.vpc.vpc_id}"
}

# shared security group for SSH
module "web-public-ssh-rule" {
  source            = "../../modules/ssh-sg"
  security_group_id = "${module.elb-sg.id}"
}

# security group rule for elb open inbound http
module "elb-http-rule" {
  source            = "../../modules/single-port-sg"
  port              = 80
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${module.elb-sg.id}"
  description       = "open HTTP on the ELB to public access"
}

# security group rule for elb open egress (outbound from nodes)
module "elb-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.elb-sg.id}"
}

# allow HTTP from ELB to web instances
module "web-http-elb-sg-rule" {
  source            = "../../modules/single-port-sg"
  port              = "3000"
  description       = "Allow ELB HTTP to web app on port 3000"
  cidr_blocks       = ["${module.vpc.public_cidr_blocks}"]
  security_group_id = "${module.elb-sg.id}"
}

# Load Balancer
resource "aws_elb" "web" {
  name = "${var.name}-elb"

  health_check {
    healthy_threshold   = 2
    interval            = 15
    target              = "TCP:3000"
    timeout             = "5"
    unhealthy_threshold = 10
  }

  # public, or private to VPC?
  internal = false

  # route HTTPS to services app on port 3000
  listener {
    instance_port     = 3000
    instance_protocol = "http"
    lb_port           = 80
    lb_protocol       = "http"
  }

  # Ensure we allow incoming traffic to the ELB, HTTP/S
  security_groups = ["${module.elb-sg.id}"]

  # ELBs in the public subnets, separate from the web ASG in private subnets
  subnets = ["${module.vpc.public_subnet_ids}"]
}

# SNS topic for the lifecycle hook
resource "aws_sns_topic" "main" {
  name = "${var.name_prefix}-lifecycle"
}

module "lifecycle-eg" {
  source         = "../../modules/lifecycled"
  azs            = "${local.azs}"
  elb_names      = ["${aws_elb.web.name}"]
  elb_arn        = "${aws_elb.web.arn}"
  name_prefix    = "${var.lifecycle_name_prefix}"
  vpc_id         = "${module.vpc.vpc_id}"
  subnet_ids     = ["${module.vpc.public_subnet_ids}"]
  elb_sg_id      = "${module.elb-sg.id}"
  instance_ami   = "${data.aws_ami.linux2.id}"
  instance_count = "2"
  instance_type  = "t2.nano"
  instance_key   = "${aws_key_pair.main.key_name}"
  asg_template_file = "${data.template_file.main.rendered}"
  sns_topic_arn = "${aws_sns_topic.main.arn}"

  tags = {
    environment = "dev"
    terraform   = "True"
  }
}

locals {
  az_count = "${length(var.public_subnet_cidrs)}"
  azs      = ["${slice(data.aws_availability_zones.available.names, 0, local.az_count)}"]
}
