/**
 * ## Run Tests on the VPC Scenario 2 Module
 *
 *
 */

variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "pm-vpc-2-alb"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "eu-west-1"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.23.0.0/16"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

variable "public_subnet_cidrs" {
  default     = ["10.23.11.0/24", "10.23.12.0/24", "10.23.13.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC"
}

variable "private_subnet_cidrs" {
  default     = ["10.23.21.0/24", "10.23.22.0/24", "10.23.23.0/24"]
  description = "A list of private subnet CIDRs to deploy inside the VPC"
}

variable "alb_security_group_ids" {
  default     = []
  description = "Extra security groups to attach in the ALB"
}

provider "aws" {
  region = var.region
}

data "aws_availability_zones" "available" {
}

module "vpc" {
  source      = "../../modules/vpc-scenario-2"
  name_prefix = var.name
  region      = var.region
  cidr        = var.vpc_cidr
  azs         = local.azs

  extra_tags = {
    kali = "ma"
  }

  public_subnet_cidrs  = var.public_subnet_cidrs
  private_subnet_cidrs = var.private_subnet_cidrs
}

module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "18.04"
}

resource "aws_key_pair" "main" {
  key_name   = var.name
  public_key = file(var.ssh_pubkey)
}

# Security group for the elastic load balancer
module "alb-sg" {
  source      = "../../modules/security-group-base"
  description = "Allow public access to ALB in ${var.name}"
  name        = "${var.name}-alb"
  vpc_id      = module.vpc.vpc_id
}

# security group rule for elb open inbound http
module "alb-http-rule" {
  source            = "../../modules/single-port-sg"
  port              = 80
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = module.alb-sg.id
  description       = "open HTTP on the ALB to public access"
}

# security group rule for elb open egress (outbound from nodes)
module "alb-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = module.alb-sg.id
}

# Security group for the web instance, only accessible from ALB
module "web-sg" {
  source      = "../../modules/security-group-base"
  description = "Allow HTTP and SSH to web instance in ${var.name}"
  name        = "${var.name}-web"
  vpc_id      = module.vpc.vpc_id
}

# allow HTTP from ELB to web instances
module "web-http-elb-sg-rule" {
  source            = "../../modules/single-port-sg"
  port              = "3000"
  description       = "Allow ELB HTTP to web app on port 3000"
  cidr_blocks       = module.vpc.public_cidr_blocks
  security_group_id = module.web-sg.id
}

# allow SSH from bastion in public subnets to web instances
module "web-ssh-rule" {
  source            = "../../modules/ssh-sg"
  cidr_blocks       = module.vpc.public_cidr_blocks #["0.0.0.0/0"] #
  security_group_id = "${module.web-sg.id}"
}

# open egress for web instances (outbound from nodes)
module "web-open-egress-sg-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = module.web-sg.id
}


## Load Balancer
# Application Load Balancer
resource "aws_lb" "web" {
  name               = "${var.name}-alb"
  idle_timeout       = "300"
  internal           = false
  load_balancer_type = "application"
  security_groups    = concat(var.alb_security_group_ids, list(module.alb-sg.id))
  subnets            = module.vpc.public_subnet_ids

  tags =  merge(
            map("Name", "${var.name}-alb"),
            "${var.extra_tags}"
          )

# TODO: setup S3 to upload logs
#  access_logs {
#    bucket  = "${var.access_log_bucket_name}"
#    enabled = true
#    prefix  = "${var.name}-alb"
#  }
}

# ALB Target group for the service running
resource "aws_lb_target_group" "alb_target_group" {
  port     = "3000"
  protocol = "HTTP"
  vpc_id   = module.vpc.vpc_id

  tags = merge(
          map("Name", "${var.name}-alb-tg"),
          "${var.extra_tags}"
         )

  health_check {
    protocol = "HTTP"
    matcher  = "200"
  }

  lifecycle {
    create_before_destroy = true
  }

  stickiness {
    type    = "lb_cookie"
    enabled = false
  }
}

# External HTTP listener on port 80 to attach the ALB
resource "aws_lb_listener" "external-http" {
  load_balancer_arn = aws_lb.web.arn
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.alb_target_group.arn
  }
}

module "web" {
  source                  = "../../modules/asg"
  ami                     = module.ubuntu-xenial-ami.id
  azs                     = local.azs
  name_prefix             = "${var.name}-web"
  alb_target_group_arns   = [aws_lb_target_group.alb_target_group.arn]
  instance_type           = "t2.nano"
  max_nodes               = length(module.vpc.public_subnet_ids)
  min_nodes               = length(module.vpc.public_subnet_ids)
  public_ip               = false
  key_name                = aws_key_pair.main.key_name
  subnet_ids              = module.vpc.private_subnet_ids

  security_group_ids = [module.web-sg.id]

  root_volume_type = "gp2"
  root_volume_size = "8"

  user_data = <<END_INIT
#!/bin/bash
${module.init-simple-hostname.init_snippet}
${module.init-warp-hello-world.init_snippet}
END_INIT
}

module "init-simple-hostname" {
  source = "../../modules/init-snippet-hostname-simple"

  hostname_prefix = "web"
}


module "init-warp-hello-world" {
  source = "../../modules/init-snippet-exec"

  init = <<END_INIT
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
  alb_logs_bucket_name = "${var.name}-alb-logs"
  az_count = length(var.public_subnet_cidrs)
  azs = slice(
    data.aws_availability_zones.available.names,
    0,
    local.az_count)
}

output "alb_dns" {
  value       = aws_lb.web.dns_name
  description = "URL, where to find the ELB"
}

output "asg_name" {
  value       = module.web.name
  description = "Name of the web ASG, for looking up IP addresses"
}

output "region" {
  value       = var.region
  description = "Region we deployed to"
}

output "bastion_eip" {
  value       = aws_eip.bastion.public_ip
  description = "EIP of the bastion host"
}
