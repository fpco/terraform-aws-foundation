/**
 * ## Run Tests on the VPC Scenario 3 Module
 *
 *
 */

variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "pm-remote-vpc-scenario-3"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-west-2"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.130.0.0/16"
}

variable "aws_availability_zones" {
  description = "List of availability zones to use. Should match number of CIDR blocks"
  default     = "us-west-2a"
}

variable "vpc_public_subnet_cidrs" {
  description = "list of CIDR ranges for the public subnets"
  default     = ["10.130.10.0/24"]
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

variable "openvpn_ami_owner_id" {
  description = "OpenVPN Access Server AMI id trial version (5 devices)"
  default     = "679593333241"
}

provider "aws" {
  region = "${var.region}"
}


module "vpc" {
  source      = "../../../modules/vpc"
  name_prefix = "${var.name}-vpc"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
}

module "vpc-public-subnets" {
  source      = "../../../modules/subnets"
  azs         = ["${var.aws_availability_zones}"]
  vpc_id      = "${module.vpc.vpc_id}"
  name_prefix = "${var.name}-vpc-public"
  cidr_blocks = "${var.vpc_public_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
}

module "vpc-sg" {
  source      = "../../../modules/security-group-base"
  description = "Test project security group"
  name        = "${var.name}-vpc-sg"
  vpc_id      = "${module.vpc.vpc_id}"
}

module "vpc-open-ssh" {
  source = "../../../modules/ssh-sg"

  # this is actually used as a name-prefix
  security_group_id = "${module.vpc-sg.id}"
}

module "vpc-open-egress" {
  source = "../../../modules/open-egress-sg"

  # this is actually used as a name-prefix
  security_group_id = "${module.vpc-sg.id}"
}

module "vpc-public-gateway" {
  source            = "../../../modules/route-public"
  vpc_id            = "${module.vpc.vpc_id}"
  name_prefix       = "${var.name}-vpc-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${concat(module.vpc-public-subnets.ids)}"]
}

# EC2 Instances setup
module "ubuntu-xenial-ami" {
  source  = "../../../modules/ami-ubuntu"
  release = "14.04"
}

data "aws_ami" "openvpn-ami" {
  most_recent = true

  filter {
    name   = "name"
    values = ["*3b5882c4-551b-43fa-acfe-7f5cdb896ff1*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["${var.openvpn_ami_owner_id}"]
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

resource "aws_instance" "vpn-machine" {
  # setup openvpn ami
  ami               = "${data.aws_ami.openvpn-ami.id}"
  count             = "1"
  key_name          = "${aws_key_pair.main.key_name}"
  instance_type     = "t2.nano"
  availability_zone = "${var.aws_availability_zones}"

  root_block_device {
    volume_type = "gp2"
    volume_size = "8"
  }

  associate_public_ip_address = "true"
  vpc_security_group_ids      = ["${module.vpc-sg.id}"]
  subnet_id                   = "${element(module.vpc-public-subnets.ids, count.index)}"

  tags {
    Name = "${var.name}-vpn-server-${count.index}"
  }

  provisioner "remote-exec" {
    connection {
      type        = "ssh"
      user        = "openvpnas"
      private_key = "${file(var.ssh_key)}"
    }
  }

  user_data = <<END_INIT
#!/bin/bash

apt-get install -y strongswan-starter

curl -s -L -o /sbin/ipsec.sh  https://docs.openvpn.net/wp-content/uploads/ipsec.sh
chmod +x /sbin/ipsec.sh

/usr/local/openvpn_as/bin/ovpn-init tool --force --batch

echo -e "openvpn\nopenvpn" | passwd openvpn
echo "Installation complte" > /tmp/openvpn-tf.log

END_INIT

}

output "openvpn-public-eip" {
  value = "${aws_instance.vpn-machine.public_ip}"
  description = "OpenVPN Public IP"
}
