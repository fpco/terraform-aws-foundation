provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

locals {
  az_count = "${length(var.public_subnet_cidrs)}"
  azs      = ["${slice(data.aws_availability_zones.available.names, 0, local.az_count)}"]
}

module "vpc" {
  source      = "../../modules/vpc-scenario-1"
  name_prefix = "${var.name}"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  azs         = ["${slice(data.aws_availability_zones.available.names, 0, 3)}"]

  public_subnet_cidrs = ["${var.public_subnet_cidrs}"]
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
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

# allow SSH
module "ssh-rule" {
  source            = "../../modules/ssh-sg"
  security_group_id = "${module.web-sg.id}"
}

# allow HTTP from ELB to web instances
module "web-http-elb-sg-rule" {
  source            = "../../modules/single-port-sg"
  port              = "8080"
  description       = "Allow ELB HTTP to web app on port 8080"
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
    target              = "TCP:8080"
    timeout             = "5"
    unhealthy_threshold = 10
  }

  # public, or private to VPC?
  internal = false

  # route HTTP to services app on port 8080
  listener {
    instance_port     = 8080
    instance_protocol = "http"
    lb_port           = 80
    lb_protocol       = "http"
  }

  # Ensure we allow incoming traffic to the ELB, HTTP
  security_groups = ["${module.elb-sg.id}"]

  # ELBs in the public subnets, separate from the web ASG in private subnets
  subnets = ["${module.vpc.public_subnet_ids}"]
}

module "ubuntu-xenial-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "16.04"
}

module "web-asg" {
  source            = "../../modules/asg"
  ami               = "${module.ubuntu-xenial-ami.id}"
  azs               = "${local.azs}"
  name_prefix       = "${var.name}-${var.web_app_name}"
  elb_names         = ["${aws_elb.web.name}"]
  instance_type     = "${var.instance_type}"
  max_nodes         = "10"
  min_nodes         = "2"
  public_ip         = false
  key_name          = "${aws_key_pair.main.key_name}"
  subnet_ids        = ["${module.vpc.public_subnet_ids}"]

  security_group_ids = ["${module.web-sg.id}"]

  root_volume_type  = "gp2"
  root_volume_size  = "8"

  user_data = <<END_INIT
#!/bin/bash
echo "hello!"
apt-get upgrade
cd /root
cat > index.html <<EOF
<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    </head>
    <body>
        <br/>
        <h2>AWS Auto-Scale Group Demo App</h2>
        <br/>
        <h4>Server local IP addresses:</h4>
        <br/>
        <strong>
EOF
echo `ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'` | cat >> index.html
cat >> index.html <<EOF
        </strong>
    </body>
</html>
EOF
curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
apt-get install -y nodejs
npm install http-server -g
apt-get install stress
http-server -p 8080
END_INIT
}


## Autoscaling Policies

# ASG Policy Up

resource "aws_autoscaling_policy" "cpu_up" {
  name = "${var.name}-asg-policy-cpu-up"
  autoscaling_group_name = "${module.web-asg.name}"
  adjustment_type     = "ChangeInCapacity"
  cooldown            = 300
  scaling_adjustment  = 3
  policy_type         = "SimpleScaling"
}

resource "aws_autoscaling_policy" "mem_up" {
  name = "${var.name}-asg-policy-mem-up"
  autoscaling_group_name = "${module.web-asg.name}"
  adjustment_type     = "ChangeInCapacity"
  cooldown            = 300
  scaling_adjustment  = 3
  policy_type         = "SimpleScaling"
}

# Cloudwatch Monitor CPU Up
resource "aws_cloudwatch_metric_alarm" "web_cpu_scale_up" {
  alarm_name = "${var.name}-cpu-scale-up-alarm"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods = "3"
  metric_name = "CPUUtilization"
  namespace = "AWS/EC2"
  period = "60"
  statistic = "Average"
  threshold = "60"
  dimensions {
    AutoScalingGroupName = "${module.web-asg.name}"
  }
  alarm_actions = ["${aws_autoscaling_policy.cpu_up.arn}"]
}

# Cloudwatch Monitor Memory Up
resource "aws_cloudwatch_metric_alarm" "web_mem_scale_up" {
  alarm_name = "${var.name}-mem-scale-up-alarm"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods = "3"
  metric_name = "MemoryUtilization"
  namespace = "AWS/EC2"
  period = "60"
  statistic = "Average"
  threshold = "60"
  dimensions {
    AutoScalingGroupName = "${module.web-asg.name}"
  }
  alarm_actions = ["${aws_autoscaling_policy.mem_up.arn}"]
}

# ASG Policy Down
resource "aws_autoscaling_policy" "cpu_down" {
  name = "${var.name}-asg-policy-cpu-down"
  autoscaling_group_name = "${module.web-asg.name}"
  adjustment_type     = "ChangeInCapacity"
  cooldown            = 300
  scaling_adjustment  = -2
  policy_type         = "SimpleScaling"
}

resource "aws_autoscaling_policy" "mem_down" {
  name = "${var.name}-asg-policy-mem-down"
  autoscaling_group_name = "${module.web-asg.name}"
  adjustment_type     = "ChangeInCapacity"
  cooldown            = 300
  scaling_adjustment  = -2
  policy_type         = "SimpleScaling"
}
# Cloudwatch CPU Monitor Down
resource "aws_cloudwatch_metric_alarm" "web_cpu_scale_down" {
  alarm_name = "${var.name}-cpu-scale-down-alarm"
  comparison_operator = "LessThanOrEqualToThreshold"
  evaluation_periods = "3"
  metric_name = "CPUUtilization"
  namespace = "AWS/EC2"
  period = "60"
  statistic = "Average"
  threshold = "30"
  dimensions {
    AutoScalingGroupName = "${module.web-asg.name}"
  }
  alarm_actions = ["${aws_autoscaling_policy.cpu_down.arn}"]
}

# Cloudwatch Memory Monitor Down
resource "aws_cloudwatch_metric_alarm" "web_mem_scale_down" {
  alarm_name = "${var.name}-mem-scale-down-alarm"
  comparison_operator = "LessThanOrEqualToThreshold"
  evaluation_periods = "3"
  metric_name = "MemoryUtilization"
  namespace = "AWS/EC2"
  period = "60"
  statistic = "Average"
  threshold = "30"
  dimensions {
    AutoScalingGroupName = "${module.web-asg.name}"
  }
  alarm_actions = ["${aws_autoscaling_policy.mem_down.arn}"]
}
