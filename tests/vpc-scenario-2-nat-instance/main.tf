/**
 * ## VPC Scenario 2 w/ EC2 NAT
 *
 * Run Tests on the VPC Scenario 2 NAT module, but deploy one NAT for all AZ.
 *
 *
 */

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "test-project"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-east-2"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

variable "vpc_cidr_block" {
  default     = "10.23.0.0/16"
  description = "CIDR block to use with the VPC"
}

variable "public_subnet_cidrs" {
  default     = ["10.23.11.0/24", "10.23.12.0/24", "10.23.13.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC"
}

variable "private_subnet_cidrs" {
  default     = ["10.23.21.0/24", "10.23.22.0/24", "10.23.23.0/24"]
  description = "A list of private subnet CIDRs to deploy inside the VPC"
}

provider "aws" {
  region = "${var.region}"
}

data "aws_availability_zones" "available" {}

module "vpc" {
  source               = "../../tf-modules/vpc"
  region               = "${var.region}"
  cidr                 = "${var.vpc_cidr_block}"
  name_prefix          = "${var.name}"
}

module "public-subnets" {
  source      = "../../tf-modules/subnets"
  azs         = ["${slice(data.aws_availability_zones.available.names, 0, 3)}"]
  vpc_id      = "${module.vpc.vpc_id}"
  name_prefix = "${var.name}-public"
  cidr_blocks = "${var.public_subnet_cidrs}"
}

module "public-gateway" {
  source            = "../../tf-modules/route-public"
  vpc_id            = "${module.vpc.vpc_id}"
  name_prefix       = "${var.name}-public"
  public_subnet_ids = ["${module.public-subnets.ids}"]
}

module "private-subnets" {
  source      = "../../tf-modules/subnets"
  azs         = ["${slice(data.aws_availability_zones.available.names, 0, 3)}"]
  vpc_id      = "${module.vpc.vpc_id}"
  name_prefix = "${var.name}-private"
  cidr_blocks = "${var.private_subnet_cidrs}"
  public      = false
}

module "nat-instance" {
  source               = "../../tf-modules/ec2-nat-instance"
  name_prefix          = "${var.name}"
  key_name             = "${aws_key_pair.main.key_name}"
  public_subnet_ids    = ["${module.public-subnets.ids[0]}"]
  # the one instance can route for any private subnet
  private_subnet_cidrs = ["${module.private-subnets.cidr_blocks}"]
  security_group_ids   = ["${aws_security_group.nat_instance.id}"]
}

# route table for the private subnets, hooks up the VPN gateway
resource "aws_route_table" "private_subnets" {
  vpc_id = "${module.vpc.vpc_id}"
  tags   = "${map("Name", "${var.name}-private-subnets")}"
}

# associate subnets to routing table
resource "aws_route_table_association" "private_subnets" {
  count          = "${length(module.private-subnets.ids)}"
  subnet_id      = "${module.private-subnets.ids[count.index]}"
  route_table_id = "${aws_route_table.private_subnets.id}"
}

# network route for private subnets ---> NAT for 0.0.0.0/0
resource "aws_route" "nat" {
  instance_id             = "${module.nat-instance.instance_ids[0]}"
  route_table_id          = "${aws_route_table.private_subnets.id}"
  destination_cidr_block  = "0.0.0.0/0"
}

# Security Group for NAT instance
resource "aws_security_group" "nat_instance" {
    name = "${var.name}-nat-instance"
    description = "Allow HTTP/HTTPS thru the NAT"
    vpc_id = "${module.vpc.vpc_id}"
}

module "nat-http-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 80
  description       = "allow ingress, HTTP (80) for NAT"
  cidr_blocks       = ["${module.private-subnets.cidr_blocks}"]
  security_group_id = "${aws_security_group.nat_instance.id}"
}

module "nat-https-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 443
  description       = "allow ingress, HTTPS (443) for NAT"
  cidr_blocks       = ["${module.private-subnets.cidr_blocks}"]
  security_group_id = "${aws_security_group.nat_instance.id}"
}

module "nat-public-ssh-rule" {
  source            = "../../tf-modules/ssh-sg"
  security_group_id = "${aws_security_group.nat_instance.id}"
}

module "nat-instance-open-egress-rule" {
  source            = "../../tf-modules/open-egress-sg"
  security_group_id = "${aws_security_group.nat_instance.id}"
}

module "ubuntu-xenial-ami" {
  source  = "../../tf-modules/ami-ubuntu"
  release = "16.04"
} 

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

# Security Group for ELB, gets public access
resource "aws_security_group" "public_elb" {
    name = "${var.name}-public-elb"
    description = "Allow public access to ELB"
    vpc_id = "${module.vpc.vpc_id}"
}

module "elb-http-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 80
  description       = "allow ingress, HTTP (80) into ELB"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.public_elb.id}"
}

module "elb-open-egress-rule" {
  source            = "../../tf-modules/open-egress-sg"
  security_group_id = "${aws_security_group.public_elb.id}"
}

# Security Group for webapp ASG, only accessible from ELB
resource "aws_security_group" "web_service" {
    name = "${var.name}-web-service"
    description = "security group for web-service instances in the private subnet"
    vpc_id = "${module.vpc.vpc_id}"
}

module "web-service-http-rule" {
  source            = "../../tf-modules/single-port-sg"
  port              = 3000
  description       = "allow ingress, HTTP port 3000 for the web app service"
  cidr_blocks       = ["${module.public-subnets.cidr_blocks}"]
  security_group_id = "${aws_security_group.web_service.id}"
}

module "web-service-vpc-ssh-rule" {
  source            = "../../tf-modules/ssh-sg"
  cidr_blocks       = ["${var.vpc_cidr_block}"]
  security_group_id = "${aws_security_group.web_service.id}"
}

module "web-service-open-egress-rule" {
  source            = "../../tf-modules/open-egress-sg"
  security_group_id = "${aws_security_group.web_service.id}"
}

resource "aws_elb" "web" {
    name = "${var.name}-public-elb"
    health_check {
        healthy_threshold = 2
        interval = 15
        target = "TCP:3000"
        timeout = "5"
        unhealthy_threshold = 10
    }
    # public, or private to VPC?
    internal = false
    # route HTTPS to services app on port 8000
    listener {
        instance_port = 3000
        instance_protocol = "http"
        lb_port = 80
        lb_protocol = "http"
    }
    # Ensure we allow incoming traffic to the ELB, HTTP/S
    security_groups = ["${aws_security_group.public_elb.id}"]
    # ELBs in the public subnets, separate from the web ASG in private subnets
    subnets = ["${module.public-subnets.ids}"]
}

module "web" {
  source             = "../../tf-modules/asg"
  ami                = "${module.ubuntu-xenial-ami.id}"
  azs                = "${slice(data.aws_availability_zones.available.names, 0, 3)}"
  name               = "${var.name}-web"
  elb_names          = ["${aws_elb.web.name}"]
  instance_type      = "t2.nano"
  desired_capacity   = "${length(module.public-subnets.ids)}"
  max_nodes          = "${length(module.public-subnets.ids)}"
  min_nodes          = "${length(module.public-subnets.ids)}"
  public_ip          = false
  key_name           = "${aws_key_pair.main.key_name}"
  subnet_ids         = ["${module.private-subnets.ids}"]
  security_group_ids = ["${aws_security_group.web_service.id}"]
  root_volume_type   = "gp2"
  root_volume_size   = "8"

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

# Security Group for bastion instance
#resource "aws_security_group" "bastion" {
#    name = "${var.name}-bastion-instance"
#    description = "Allow SSH to bastion"
#    vpc_id = "${module.vpc.vpc_id}"
#}
#
#module "bastion-public-ssh-rule" {
#  source            = "../../tf-modules/ssh-sg"
#  security_group_id = "${aws_security_group.nat_instance.id}"
#}
#
#module "bastion-open-egress-rule" {
#  source            = "../../tf-modules/open-egress-sg"
#  security_group_id = "${aws_security_group.nat_instance.id}"
#}
#
#resource "aws_instance" "bastion" {
#  ami               = "${module.ubuntu-xenial-ami.id}"
#  key_name          = "${aws_key_pair.main.key_name}"
#  instance_type     = "t2.nano"
#  availability_zone = "${data.aws_availability_zones.available.names[0]}"
#  #availability_zone = "${join("", slice(data.aws_availability_zones.available.names, 0, 1))}"
#  root_block_device {
#    volume_type = "gp2"
#    volume_size = "10"
#  }
#  associate_public_ip_address = "true"
#  vpc_security_group_ids      = ["${module.public-ssh-sg.id}",
#                                 "${module.open-egress-sg.id}"
#  ]
#  subnet_id = "${module.vpc.public_subnet_ids[0]}"
#  tags {
#    Name = "${var.name}-bastion"
#  }
#}

// make the ELB accessible on the outside
output "elb_dns" {
  value = "${aws_elb.web.dns_name}"
}

// Public IP of NAT instance
output "nat_ip" {
  value = "${module.nat-instance.public_ips[0]}"
}

// region deployed to
output "region" {
  value = "${var.region}"
}

// name of the web service autoscaling group
output "web_asg_name" {
  value = "${var.name}-web-cluster"
}
