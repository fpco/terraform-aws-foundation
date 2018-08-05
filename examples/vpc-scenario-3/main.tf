/**
 * ## Run Tests on the VPC Scenario 3 Module
 *
 *
 */

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "vpc" {
  source      = "../../modules/vpc-scenario-3"
  name_prefix = "${var.name}"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  azs         = ["${local.azs}"]

  extra_tags = {
    kali = "ma"
  }

  public_subnet_cidrs  = ["${var.public_subnet_cidrs}"]
  private_subnet_cidrs = ["${var.private_subnet_cidrs}"]

  vpn_static_routes = ["${var.vpn_static_routes}"]
  vpn_remote_ip     = "${var.vpn_remote_ip}"
}

module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "14.04"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

# SSH Security group
module "private-ssh-sg" {
  source      = "../../modules/security-group-base"
  description = "Allow Private ssh access in ${var.name}"
  name        = "${var.name}-private-ssh"
  vpc_id      = "${module.vpc.vpc_id}"
}

module "ssh-ingress-rule" {
  source              = "../../modules/ssh-sg"
  security_group_id   = "${module.private-ssh-sg.id}"
}

# open egress for web instances (outbound from nodes)
module "ssh-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.private-ssh-sg.id}"
}

# Security group for the elastic load balancer
module "elb-sg" {
  source      = "../../modules/security-group-base"
  description = "Allow public access to ELB in ${var.name}"
  name        = "${var.name}-elb"
  vpc_id      = "${module.vpc.vpc_id}"
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

# Security group for the web instance, only accessible from ELB
module "web-sg" {
  source      = "../../modules/security-group-base"
  description = "Allow HTTP and SSH to web instance in ${var.name}"
  name        = "${var.name}-web"
  vpc_id      = "${module.vpc.vpc_id}"
}

# allow HTTP from ELB to web instances
module "web-http-elb-sg-rule" {
  source            = "../../modules/single-port-sg"
  port              = "3000"
  description       = "Allow ELB HTTP to web app on port 3000"
  cidr_blocks       = ["${module.vpc.public_cidr_blocks}"]
  security_group_id = "${module.web-sg.id}"
}

# open egress for web instances (outbound from nodes)
module "web-open-egress-sg-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.web-sg.id}"
}

# Load Balancer
resource "aws_elb" "web" {
  name = "${var.name}-public-elb"

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

module "web" {
  source        = "../../modules/asg"
  ami           = "${module.ubuntu-xenial-ami.id}"
  azs           = "${local.azs}"
  name_prefix   = "${var.name}-web"
  elb_names     = ["${aws_elb.web.name}"]
  instance_type = "t2.nano"
  max_nodes     = "${length(module.vpc.public_subnet_ids)}"
  min_nodes     = "${length(module.vpc.public_subnet_ids)}"
  public_ip     = false
  key_name      = "${aws_key_pair.main.key_name}"
  subnet_ids    = ["${module.vpc.private_subnet_ids}"]

  security_group_ids = ["${module.web-sg.id}","${module.private-ssh-sg.id}"]

  root_volume_type = "gp2"
  root_volume_size = "8"

  user_data = <<END_INIT
#!/bin/bash
echo "hello!"
apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) \
    stable"
apt-get update
apt-get install -y docker-ce

docker run                   \
    --restart=always         \
    -d                       \
    -v /var/log/cloud-init-output.log:/var/www/html/cloud-init-output.log \
    -p $(ec2metadata --local-ipv4):3000:3000 \
    yesodweb/warp            \
    warp --docroot /var/www/html

END_INIT
}

locals {
  az_count = "${length(var.public_subnet_cidrs)}"
  azs      = ["${slice(data.aws_availability_zones.available.names, 0, local.az_count)}"]
}
